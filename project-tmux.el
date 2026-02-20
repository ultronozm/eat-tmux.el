;;; project-tmux.el --- Project tmux views in Eat -*- lexical-binding: t; -*-

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

;; `project-tmux' opens project-scoped tmux views inside Eat buffers.
;;
;; Prefix argument behavior:
;; - no prefix: attach/create view 1
;; - numeric prefix N: attach/create view N
;; - non-numeric prefix (for example C-u): create next free view index
;;
;; Example setup:
;;
;; (add-to-list 'load-path "/path/to/project-tmux/")
;; (require 'project-tmux)
;; (require 'project)
;; (keymap-set project-prefix-map "t" #'project-tmux)
;; (add-to-list 'project-switch-commands '(project-tmux "Tmux" nil))
;;
;; In `project-tmux' buffers, `project-tmux-mode' binds:
;; - C-c C-v: `project-tmux-capture-pane'

;;; Code:

(require 'subr-x)

(declare-function eat "eat" (&optional program arg))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-prefixed-buffer-name "project" (mode))
(declare-function project-root "project" (project))

(defvar eat-buffer-name)
(defvar-local project-tmux--session-base nil)
(defvar-local project-tmux--view-index nil)
(defvar project-tmux-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-v" #'project-tmux-capture-pane)
    map)
  "Keymap for `project-tmux-mode'.")

(defgroup project-tmux nil
  "Project-local tmux views in Eat."
  :group 'tools
  :prefix "project-tmux-")

(define-minor-mode project-tmux-mode
  "Minor mode enabled in buffers created by `project-tmux'."
  :init-value nil
  :lighter nil
  :keymap project-tmux-mode-map)

(defcustom project-tmux-identity-strategy 'path
  "How `project-tmux' computes the base tmux session name.

The value can be one of:

- `path': derive the session name from project root path (compatible
  with `tmux-here' naming).
- `id-file': read or create `.project-tmux-id' in project root and use
  that value as the base session name.
- FUNCTION: called with one argument PROJECT, and should return a
  non-empty session base name string."
  :type '(choice
          (const :tag "Project root path" path)
          (const :tag "Stable ID file" id-file)
          (function :tag "Custom function"))
  :group 'project-tmux)

(defconst project-tmux--id-file-name ".project-tmux-id"
  "File used for stable session identity when using `id-file' strategy.")

(defun project-tmux--ensure-tmux ()
  "Ensure tmux executable is available."
  (unless (executable-find "tmux")
    (user-error "`tmux' executable was not found in PATH")))

(defun project-tmux--project-context ()
  "Return project context plist with root and session base."
  (let* ((project (project-current t))
         (root (project-root project)))
    (list :project project
          :root root
          :session-base (project-tmux--session-base-name project))))

(defun project-tmux--current-view-index (session-base)
  "Return current project-tmux view index for SESSION-BASE, defaulting to 1."
  (if (and (derived-mode-p 'eat-mode)
           (integerp project-tmux--view-index)
           (stringp project-tmux--session-base)
           (string= project-tmux--session-base session-base))
      project-tmux--view-index
    1))

(defun project-tmux--path-session-base (project)
  "Return path-based tmux session base name for PROJECT."
  (let* ((root (directory-file-name (file-truename (project-root project))))
         (base (file-name-nondirectory root))
         (hash (substring (secure-hash 'sha1 root) 0 8)))
    (format "%s-%s" base hash)))

(defun project-tmux--read-id-file (file)
  "Return FILE contents as a trimmed string, or nil if empty/unreadable."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((value (string-trim (buffer-string))))
        (unless (string-empty-p value)
          value)))))

(defun project-tmux--generate-id ()
  "Generate a stable-looking tmux session ID."
  (format "ptmx-%s"
          (substring
           (secure-hash 'sha1
                        (format "%s:%s:%s:%s:%s"
                                (float-time)
                                (emacs-pid)
                                (user-real-uid)
                                (random)
                                (system-name)))
           0 12)))

(defun project-tmux--id-file-session-base (project)
  "Return ID-file-based session base name for PROJECT.

If `.project-tmux-id' is missing, create it.  If creation fails,
fall back to path-based naming."
  (let* ((root (project-root project))
         (file (expand-file-name project-tmux--id-file-name root))
         (existing (project-tmux--read-id-file file)))
    (if existing
        existing
      (let ((created (project-tmux--generate-id)))
        (condition-case err
            (progn
              (with-temp-file file
                (insert created "\n"))
              created)
          (file-error
           (message "project-tmux: could not write %s (%s), falling back to path strategy"
                    file (error-message-string err))
           (project-tmux--path-session-base project)))))))

(defun project-tmux--session-base-name (project)
  "Return tmux base session name for PROJECT."
  (let* ((raw
          (cond
           ((eq project-tmux-identity-strategy 'path)
            (project-tmux--path-session-base project))
           ((eq project-tmux-identity-strategy 'id-file)
            (project-tmux--id-file-session-base project))
           ((functionp project-tmux-identity-strategy)
            (funcall project-tmux-identity-strategy project))
           (t
            (user-error "Invalid `project-tmux-identity-strategy': %S"
                        project-tmux-identity-strategy))))
         (name (and (stringp raw) (string-trim raw))))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "`project-tmux-identity-strategy' returned an empty session name"))
    (when (string-match-p ":" name)
      (user-error "Tmux session names for project-tmux cannot contain ':' (got %S)" name))
    name))

(defun project-tmux--view-session-name (session-base view-index)
  "Return tmux session name for SESSION-BASE and VIEW-INDEX."
  (if (= view-index 1)
      session-base
    (format "%s--view%d" session-base view-index)))

(defun project-tmux--next-view-index (session-base)
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

(defun project-tmux--command (session-base view-index directory)
  "Return shell command string for SESSION-BASE VIEW-INDEX in DIRECTORY."
  (let* ((target (project-tmux--view-session-name session-base view-index))
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

(defun project-tmux--disable-kill-query (buffer)
  "Make killing BUFFER detach tmux without a confirmation prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions
                  (remove #'process-kill-buffer-query-function
                          kill-buffer-query-functions))
      (when-let* ((proc (get-buffer-process buffer)))
        (set-process-query-on-exit-flag proc nil)))))

(defun project-tmux--capture-pane-text (pane-id)
  "Capture all available text from tmux PANE-ID."
  (with-temp-buffer
    (let ((status (process-file "tmux" nil t nil
                                "capture-pane" "-p" "-J" "-S" "-"
                                "-t" pane-id)))
      (if (zerop status)
          (buffer-string)
        (user-error "Could not capture tmux pane %s: %s"
                    pane-id (string-trim (buffer-string)))))))

(defun project-tmux--current-pane-id (session-target)
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

;;;###autoload
(defun project-tmux-capture-pane ()
  "Capture the current project-tmux pane into a `special-mode' buffer."
  (interactive)
  (require 'project)
  (project-tmux--ensure-tmux)
  (let* ((context (project-tmux--project-context))
         (session-base (plist-get context :session-base))
         (view-index (project-tmux--current-view-index session-base)))
    (let* ((target (project-tmux--view-session-name session-base view-index))
           (pane-id (project-tmux--current-pane-id target))
           (text (project-tmux--capture-pane-text pane-id))
           (base-name (project-prefixed-buffer-name "tmux-capture"))
           (buffer-name (if (= view-index 1)
                            base-name
                          (format "%s<%d>" base-name view-index)))
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
      (pop-to-buffer buffer))))

;;;###autoload
(defun project-tmux-capture-window ()
  "Backward-compatible alias for `project-tmux-capture-pane'."
  (interactive)
  (project-tmux-capture-pane))

;;;###autoload
(defun project-tmux (&optional arg)
  "Open a project-local tmux view in an Eat buffer.

With no prefix ARG, attach/create view 1.
With numeric prefix ARG N, attach/create view N.
With non-numeric prefix ARG (for example `C-u'), create the next
available view index."
  (interactive "P")
  (unless (require 'eat nil t)
    (user-error "Package `eat' is not available"))
  (require 'project)
  (project-tmux--ensure-tmux)
  (let* ((context (project-tmux--project-context))
         (default-directory (plist-get context :root))
         (session-base (plist-get context :session-base))
         (view-index (cond
                      ((numberp arg)
                       (prefix-numeric-value arg))
                      (arg
                       (project-tmux--next-view-index session-base))
                      (t 1))))
    (when (< view-index 1)
      (user-error "View index must be >= 1"))
    (let* ((command (project-tmux--command session-base
                                           view-index
                                           default-directory))
           (eat-buffer-name (project-prefixed-buffer-name "tmux"))
           (eat-arg (if arg view-index nil))
           (buffer (eat command eat-arg)))
      (with-current-buffer buffer
        (setq-local project-tmux--session-base session-base
                    project-tmux--view-index view-index)
        (project-tmux-mode 1))
      (project-tmux--disable-kill-query buffer)
      buffer)))

(provide 'project-tmux)
;;; project-tmux.el ends here
