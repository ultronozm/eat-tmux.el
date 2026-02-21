;;; eat-tmux.el --- Project tmux views in Eat -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Keywords: tools, terminals, project

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `eat-tmux' opens project-scoped tmux views inside Eat buffers.
;;
;; Prefix argument behavior:
;; - no prefix: attach/create view 1
;; - numeric prefix N: attach/create view N
;; - non-numeric prefix (for example C-u): create next free view index
;;
;; Example setup:
;;
;; (add-to-list 'load-path "/path/to/eat-tmux/")
;; (require 'eat-tmux)
;; (require 'project)
;; (keymap-set project-prefix-map "t" #'eat-tmux)
;; (add-to-list 'project-switch-commands '(eat-tmux "Tmux" nil))
;;
;; In `eat-tmux' buffers, `eat-tmux-mode' binds:
;; - C-c C-v: `eat-tmux-capture-pane'

;;; Code:

(require 'subr-x)

(declare-function eat "eat" (&optional program arg))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-prefixed-buffer-name "project" (mode))
(declare-function project-root "project" (project))

(defvar eat-buffer-name)
(defvar-local eat-tmux--session-base nil)
(defvar-local eat-tmux--view-index nil)
(defvar eat-tmux-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-v" #'eat-tmux-capture-pane)
    map)
  "Keymap for `eat-tmux-mode'.")

(defgroup eat-tmux nil
  "Project-local tmux views in Eat."
  :group 'tools
  :prefix "eat-tmux-")

(define-minor-mode eat-tmux-mode
  "Minor mode enabled in buffers created by `eat-tmux'."
  :init-value nil
  :lighter nil
  :keymap eat-tmux-mode-map)

(defcustom eat-tmux-identity-strategy 'path
  "How `eat-tmux' computes the base tmux session name.

The value can be one of:

- `path': derive the session name from project root path (compatible
  with `tmux-here' naming).
- `id-file': read or create `.eat-tmux-id' in project root and use
  that value as the base session name.
- FUNCTION: called with one argument PROJECT, and should return a
  non-empty session base name string."
  :type '(choice
          (const :tag "Project root path" path)
          (const :tag "Stable ID file" id-file)
          (function :tag "Custom function"))
  :group 'eat-tmux)

(defconst eat-tmux--id-file-name ".eat-tmux-id"
  "File used for stable session identity when using `id-file' strategy.")

(defun eat-tmux--ensure-tmux ()
  "Ensure tmux executable is available."
  (unless (executable-find "tmux")
    (user-error "`tmux' executable was not found in PATH")))

(defun eat-tmux--project-context ()
  "Return project context plist with root and session base."
  (let* ((project (project-current t))
         (root (project-root project)))
    (list :project project
          :root root
          :session-base (eat-tmux--session-base-name project))))

(defun eat-tmux--current-view-index (session-base)
  "Return current eat-tmux view index for SESSION-BASE, defaulting to 1."
  (if (and (derived-mode-p 'eat-mode)
           (integerp eat-tmux--view-index)
           (stringp eat-tmux--session-base)
           (string= eat-tmux--session-base session-base))
      eat-tmux--view-index
    1))

(defun eat-tmux--normalize-tty (tty)
  "Normalize TTY path/name for tmux client matching."
  (when (and (stringp tty) (not (string-empty-p tty)))
    (if (string-prefix-p "/dev/" tty)
        (substring tty (length "/dev/"))
      (file-name-nondirectory tty))))

(defun eat-tmux--tmux-clients ()
  "Return tmux clients as a list of plists, or nil on failure."
  (with-temp-buffer
    (let ((status (process-file "tmux" nil t nil
                                "list-clients" "-F"
                                "#{client_pid}\t#{client_tty}\t#{client_name}\t#{client_session}\t#{pane_id}")))
      (when (zerop status)
        (let ((clients nil))
          (dolist (line (split-string (buffer-string) "\n" t))
            (pcase-let ((`(,pid-str ,tty-str ,name ,session ,pane-id . ,_)
                         (split-string line "\t")))
              (when (and pid-str tty-str name session pane-id)
                (push (list :pid (and (string-match-p "\\`[0-9]+\\'" pid-str)
                                      (string-to-number pid-str))
                            :tty (eat-tmux--normalize-tty tty-str)
                            :name name
                            :session session
                            :pane pane-id)
                      clients))))
          (nreverse clients))))))

(defun eat-tmux--tmux-client-for-process (process)
  "Return tmux client plist associated with PROCESS, if any."
  (when (and process (process-live-p process))
    (let ((pid (process-id process))
          (tty (eat-tmux--normalize-tty
                (ignore-errors (process-tty-name process))))
          pid-match tty-match)
      (dolist (client (eat-tmux--tmux-clients))
        (when (and (null pid-match)
                   pid
                   (equal pid (plist-get client :pid)))
          (setq pid-match client))
        (when (and (null tty-match)
                   tty
                   (string= tty (plist-get client :tty)))
          (setq tty-match client)))
      (or pid-match tty-match))))

(defun eat-tmux--session-name-components (session-name)
  "Return (SESSION-BASE VIEW-INDEX) parsed from SESSION-NAME."
  (if (string-match "\\`\\(.*\\)\\(?:--view\\|:view\\)\\([0-9]+\\)\\'" session-name)
      (list (match-string 1 session-name)
            (string-to-number (match-string 2 session-name)))
    (list session-name 1)))

(defun eat-tmux--path-session-base (project)
  "Return path-based tmux session base name for PROJECT."
  (let* ((root (directory-file-name (file-truename (project-root project))))
         (base (file-name-nondirectory root))
         (hash (substring (secure-hash 'sha1 root) 0 8)))
    (format "%s-%s" base hash)))

(defun eat-tmux--read-id-file (file)
  "Return FILE contents as a trimmed string, or nil if empty/unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((value (string-trim (buffer-string))))
        (unless (string-empty-p value)
          value)))))

(defun eat-tmux--generate-id ()
  "Generate a stable-looking tmux session ID."
  (format "etmx-%s"
          (substring
           (secure-hash 'sha1
                        (format "%s:%s:%s:%s:%s"
                                (float-time)
                                (emacs-pid)
                                (user-real-uid)
                                (random)
                                (system-name)))
           0 12)))

(defun eat-tmux--id-file-session-base (project)
  "Return ID-file-based session base name for PROJECT.

If `.eat-tmux-id' is missing, create it.  If creation fails,
fall back to path-based naming."
  (let* ((root (project-root project))
         (file (expand-file-name eat-tmux--id-file-name root))
         (existing (eat-tmux--read-id-file file)))
    (if existing
        existing
      (let ((created (eat-tmux--generate-id)))
        (condition-case err
            (progn
              (with-temp-file file
                (insert created "\n"))
              created)
          (file-error
           (message "eat-tmux: could not write %s (%s), falling back to path strategy"
                    file (error-message-string err))
           (eat-tmux--path-session-base project)))))))

(defun eat-tmux--session-base-name (project)
  "Return tmux base session name for PROJECT."
  (let* ((raw
          (cond
           ((eq eat-tmux-identity-strategy 'path)
            (eat-tmux--path-session-base project))
           ((eq eat-tmux-identity-strategy 'id-file)
            (eat-tmux--id-file-session-base project))
           ((functionp eat-tmux-identity-strategy)
            (funcall eat-tmux-identity-strategy project))
           (t
            (user-error "Invalid `eat-tmux-identity-strategy': %S"
                        eat-tmux-identity-strategy))))
         (name (and (stringp raw) (string-trim raw))))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "`eat-tmux-identity-strategy' returned an empty session name"))
    (when (string-match-p ":" name)
      (user-error "Tmux session names for eat-tmux cannot contain ':' (got %S)" name))
    name))

(defun eat-tmux--view-session-name (session-base view-index)
  "Return tmux session name for SESSION-BASE and VIEW-INDEX."
  (if (= view-index 1)
      session-base
    (format "%s--view%d" session-base view-index)))

(defun eat-tmux--next-view-index (session-base)
  "Return the next free view index for SESSION-BASE.

SESSION-BASE itself counts as view 1."
  (let ((re (format "\\`%s\\(?:--view\\|:view\\)\\([0-9]+\\)\\'"
                    (regexp-quote session-base)))
        (max-view 1))
    (dolist (name (condition-case nil
                      (process-lines "tmux" "list-sessions" "-F" "#{session_name}")
                    (error nil)))
      (cond
       ((string= name session-base)
        (setq max-view (max max-view 1)))
       ((string-match re name)
        (setq max-view (max max-view
                            (string-to-number (match-string 1 name)))))))
    (1+ max-view)))

(defun eat-tmux--command (session-base view-index directory)
  "Return shell command string for SESSION-BASE VIEW-INDEX in DIRECTORY."
  (let* ((target (eat-tmux--view-session-name session-base view-index))
         (q #'shell-quote-argument)
         (parts
          (list
           (format "tmux has-session -t %s 2>/dev/null || tmux new-session -Ad -s %s -c %s"
                   (funcall q session-base)
                   (funcall q session-base)
                   (funcall q directory))
           (unless (= view-index 1)
             (format "tmux has-session -t %s 2>/dev/null || tmux new-session -Ad -t %s -s %s -c %s"
                     (funcall q target)
                     (funcall q session-base)
                     (funcall q target)
                     (funcall q directory)))
           (format "if [ -n \"$TMUX\" ]; then tmux switch-client -t %s; else exec tmux attach-session -t %s; fi"
                   (funcall q target)
                   (funcall q target)))))
    (mapconcat #'identity (delq nil parts) "; ")))

(defun eat-tmux--disable-kill-query (buffer)
  "Make killing BUFFER detach tmux without a confirmation prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions
                  (remove #'process-kill-buffer-query-function
                          kill-buffer-query-functions))
      (when-let* ((proc (get-buffer-process buffer)))
        (set-process-query-on-exit-flag proc nil)))))

(defun eat-tmux--capture-pane-text (pane-id)
  "Capture all available text from tmux PANE-ID."
  (with-temp-buffer
    (let ((status (process-file "tmux" nil t nil
                                "capture-pane" "-p" "-J" "-S" "-"
                                "-t" pane-id)))
      (if (zerop status)
          (buffer-string)
        (user-error "Could not capture tmux pane %s: %s"
                    pane-id (string-trim (buffer-string)))))))

(defun eat-tmux--current-pane-id (session-target)
  "Return ID of active pane in current tmux window of SESSION-TARGET."
  (with-temp-buffer
    (let ((status (process-file "tmux" nil t nil
                                "display-message"
                                "-p"
                                "-t" session-target
                                "#{pane_id}")))
      (unless (zerop status)
        (user-error "Could not get current tmux pane for %s: %s"
                    session-target (string-trim (buffer-string)))))
    (let ((pane-id (string-trim (buffer-string))))
      (if (string-empty-p pane-id)
          (user-error "No active pane found for %s" session-target)
        pane-id))))

(defun eat-tmux--capture-context-from-current-buffer ()
  "Return live tmux context from current Eat buffer, or nil."
  (when (derived-mode-p 'eat-mode)
    (when-let* ((proc (get-buffer-process (current-buffer)))
                ((process-live-p proc))
                (client (eat-tmux--tmux-client-for-process proc)))
      (let* ((session-name (plist-get client :session))
             (pane-id (plist-get client :pane))
             (parts (eat-tmux--session-name-components session-name))
             (session-base (nth 0 parts))
             (view-index (nth 1 parts)))
        (unless (and (stringp pane-id) (not (string-empty-p pane-id)))
          (setq pane-id
                (eat-tmux--current-pane-id
                 (eat-tmux--view-session-name session-base view-index))))
        (list :session-base session-base
              :view-index view-index
              :pane-id pane-id)))))

(defun eat-tmux--fallback-capture-context ()
  "Return capture context based on project/session metadata."
  (let* ((context (eat-tmux--project-context))
         (session-base (plist-get context :session-base))
         (view-index (eat-tmux--current-view-index session-base))
         (target (eat-tmux--view-session-name session-base view-index))
         (pane-id (eat-tmux--current-pane-id target)))
    (list :session-base session-base
          :view-index view-index
          :pane-id pane-id)))

(defun eat-tmux--capture-buffer-name (session-base view-index)
  "Return capture buffer name for SESSION-BASE and VIEW-INDEX."
  (let ((base-name (or (and (require 'project nil t)
                            (ignore-errors
                              (project-prefixed-buffer-name "tmux-capture")))
                       (format "*tmux-capture:%s*" session-base))))
    (if (= view-index 1)
        base-name
      (format "%s<%d>" base-name view-index))))

;;;###autoload
(defun eat-tmux-capture-pane ()
  "Capture tmux output into a `special-mode' buffer.

If current buffer is an Eat buffer attached to tmux, capture from the
live pane attached to that buffer's tmux client.

Otherwise, fall back to current eat-tmux session/view metadata."
  (interactive)
  (eat-tmux--ensure-tmux)
  (let* ((capture-context
          (or (eat-tmux--capture-context-from-current-buffer)
              (progn
                (require 'project)
                (eat-tmux--fallback-capture-context))))
         (session-base (plist-get capture-context :session-base))
         (view-index (plist-get capture-context :view-index))
         (pane-id (plist-get capture-context :pane-id))
         (text (eat-tmux--capture-pane-text pane-id))
         (buffer-name (eat-tmux--capture-buffer-name session-base view-index))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Session: %s\nView: %d\nPane: %s\n\n"
                        session-base view-index pane-id))
        (insert text)
        (unless (string-suffix-p "\n" text)
          (insert "\n"))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun eat-tmux-capture-window ()
  "Backward-compatible alias for `eat-tmux-capture-pane'."
  (interactive)
  (eat-tmux-capture-pane))

;;;###autoload
(defun eat-tmux (&optional arg)
  "Open a project-local tmux view in an Eat buffer.

With no prefix ARG, attach/create view 1.
With numeric prefix ARG N, attach/create view N.
With non-numeric prefix ARG (for example `C-u'), create the next
available view index."
  (interactive "P")
  (unless (require 'eat nil t)
    (user-error "Package `eat' is not available"))
  (require 'project)
  (eat-tmux--ensure-tmux)
  (let* ((context (eat-tmux--project-context))
         (default-directory (plist-get context :root))
         (session-base (plist-get context :session-base))
         (view-index (cond
                      ((numberp arg)
                       (prefix-numeric-value arg))
                      (arg
                       (eat-tmux--next-view-index session-base))
                      (t 1))))
    (when (< view-index 1)
      (user-error "View index must be >= 1"))
    (let* ((command (eat-tmux--command session-base
                                           view-index
                                           default-directory))
           (eat-buffer-name (project-prefixed-buffer-name "tmux"))
           (eat-arg (if arg view-index nil))
           (buffer (eat command eat-arg)))
      (with-current-buffer buffer
        (setq-local eat-tmux--session-base session-base
                    eat-tmux--view-index view-index)
        (eat-tmux-mode 1))
      (eat-tmux--disable-kill-query buffer)
      buffer)))

(provide 'eat-tmux)
;;; eat-tmux.el ends here
