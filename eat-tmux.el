;;; eat-tmux.el --- Tmux views in Eat -*- lexical-binding: t; -*-

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

;; `eat-tmux-project' opens project-scoped tmux views inside Eat buffers.
;; `eat-tmux-open' opens or creates a tmux session for any directory.
;; `eat-tmux-manager' provides a tabulated session browser/jump UI.
;;
;; `eat-tmux-manager' can optionally display pane/window activity from
;; tmux environment metadata (for example variables produced by
;; tmux-agent-indicator: <https://github.com/accessd/tmux-agent-indicator>).
;;
;; `eat-tmux-project' prefix argument behavior:
;; - no prefix: attach or create view 1
;; - numeric prefix N: attach or create view N
;; - non-numeric prefix (e.g. C-u): create the next free view index
;;
;; Remote projects (TRAMP ssh/scp directories) are supported: the tmux
;; session runs on the remote host via `ssh -tt'.
;;
;; Example setup:
;;
;;   (require 'eat-tmux)
;;   (with-eval-after-load 'project
;;     (keymap-set project-prefix-map "t" #'eat-tmux-project)
;;     (add-to-list 'project-switch-commands
;;                  '(eat-tmux-project "Tmux" nil)))
;;
;; `eat-tmux-mode' (activated automatically in buffers created by
;; `eat-tmux-project' and `eat-tmux-open') binds `C-c C-v' to capture
;; the current tmux pane's text into a `special-mode' buffer.

;;; Code:

(require 'subr-x)
(require 'tabulated-list)

(declare-function eat "eat" (&optional program arg))
(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-prefixed-buffer-name "project" (mode))
(declare-function project-root "project" (project))
(declare-function tramp-dissect-file-name "tramp" (name &optional nodefault))
(declare-function tramp-file-name-host "tramp" (vec))
(declare-function tramp-file-name-localname "tramp" (vec))
(declare-function tramp-file-name-method "tramp" (vec))
(declare-function tramp-file-name-port "tramp" (vec))
(declare-function tramp-file-name-user "tramp" (vec))

(defvar eat-buffer-name)
(defvar eat--char-mode)
(defvar eat-tmux-mode)

(defvar-local eat-tmux--session-base nil)
(defvar-local eat-tmux--view-index nil)
(defvar-local eat-tmux--session-name nil
  "Exact tmux session name attached to this Eat buffer.")
(defvar-local eat-tmux--remote-context nil
  "Remote connection plist for tmux commands in current Eat buffer.")
(defvar-local eat-tmux--base-buffer-name nil
  "Base buffer name used for this tmux context across views.")
(defvar eat-tmux-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-v" #'eat-tmux-capture-pane)
    map)
  "Keymap for `eat-tmux-mode'.")

(defvar-local eat-tmux--keymap-hidden-in-char-mode nil
  "Non-nil when `eat-tmux-mode' keymap is disabled for Eat char mode.")

(defconst eat-tmux--mode-transition-functions
  '(eat--char-mode)
  "Eat functions that can switch between char and non-char submodes.")

(defun eat-tmux--apply-keymap-override ()
  "Hide `eat-tmux-mode' keymap when Eat char mode is active."
  (let ((entry (assq 'eat-tmux-mode minor-mode-overriding-map-alist)))
    (if (bound-and-true-p eat--char-mode)
        (progn
          (if entry
              (setcdr entry nil)
            (push (cons 'eat-tmux-mode nil) minor-mode-overriding-map-alist))
          (setq eat-tmux--keymap-hidden-in-char-mode t))
      (if entry
          (setcdr entry eat-tmux-mode-map)
        (setq minor-mode-overriding-map-alist
              (assq-delete-all 'eat-tmux-mode
                               minor-mode-overriding-map-alist)))
      (setq eat-tmux--keymap-hidden-in-char-mode nil))))

(defun eat-tmux--refresh-keymap-for-buffer ()
  "Refresh keymap visibility in current Eat tmux buffer."
  (when (and eat-tmux-mode (derived-mode-p 'eat-mode))
    (let ((char-mode-active (bound-and-true-p eat--char-mode)))
      (unless (eq char-mode-active eat-tmux--keymap-hidden-in-char-mode)
        (eat-tmux--apply-keymap-override)))))

(defun eat-tmux--refresh-keymap-after-mode-change (&rest _)
  "After-advice to refresh Eat tmux keymap when Eat mode changes."
  (eat-tmux--refresh-keymap-for-buffer))

(defun eat-tmux--install-mode-transition-advices ()
  "Install advice to refresh keymap after Eat submode transitions."
  (dolist (fn eat-tmux--mode-transition-functions)
    (unless (advice-member-p #'eat-tmux--refresh-keymap-after-mode-change fn)
      (advice-add fn :after #'eat-tmux--refresh-keymap-after-mode-change))))

(defun eat-tmux--clear-keymap-override ()
  "Remove `eat-tmux-mode' overriding keymap entry."
  (setq minor-mode-overriding-map-alist
        (assq-delete-all 'eat-tmux-mode minor-mode-overriding-map-alist))
  (setq eat-tmux--keymap-hidden-in-char-mode nil))

(defgroup eat-tmux nil
  "Project-local tmux views in Eat."
  :group 'tools
  :prefix "eat-tmux-")

(define-minor-mode eat-tmux-mode
  "Minor mode enabled in buffers created by `eat-tmux'."
  :init-value nil
  :lighter nil
  :keymap eat-tmux-mode-map
  (if eat-tmux-mode
      (progn
        (eat-tmux--install-mode-transition-advices)
        (eat-tmux--apply-keymap-override))
    (eat-tmux--clear-keymap-override)))

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

(defcustom eat-tmux-remote-execution 'ssh
  "How `eat-tmux-project' handles remote project roots.

- `ssh': run tmux on the remote host via `ssh -tt'.
- `error': signal an error for remote projects."
  :type '(choice
          (const :tag "Run remotely over ssh" ssh)
          (const :tag "Error on remote projects" error))
  :group 'eat-tmux)

(defcustom eat-tmux-manager-include-remote t
  "When non-nil, include remote tmux sessions in `eat-tmux-manager'."
  :type 'boolean
  :group 'eat-tmux)

(defcustom eat-tmux-manager-path-column-width 24
  "Width of the Path column in `eat-tmux-manager'.

Path display is tail-truncated to keep the most specific suffix visible."
  :type 'integer
  :group 'eat-tmux)

(defcustom eat-tmux-manager-pane-state-provider
  #'eat-tmux-manager-pane-state-provider-tmux-environment
  "Function used to read pane state metadata for `eat-tmux-manager'.

The function is called with one argument REMOTE (a remote context
plist or nil for local tmux) and should return either:

- a hash table mapping pane IDs (for example \"%1\") to raw state
  strings, or
- nil to indicate no pane state metadata is available.

The default provider reads `tmux show-environment -g' lines and parses
variables like `TMUX_AGENT_PANE_%N_STATE=...'.  This intentionally
integrates with tmux-agent-indicator while remaining configurable."
  :type '(choice
          (const :tag "Disabled" nil)
          (function :tag "Provider function"))
  :group 'eat-tmux)

(defcustom eat-tmux-manager-pane-state-env-regexp
  "\\`TMUX_AGENT_PANE_\\(%[0-9]+\\)_STATE=\\(.+\\)\\'"
  "Regexp used by default pane state provider.

The regexp must capture:
- group 1: pane ID (for example \"%1\")
- group 2: raw state value."
  :type 'regexp
  :group 'eat-tmux)

(defcustom eat-tmux-manager-pane-state-normalizer
  #'eat-tmux-manager-normalize-pane-state
  "Function used to normalize raw pane state values.

The function is called with RAW-STATE (a string or nil) and should
return one of:
- \"needs-input\"
- \"running\"
- \"idle\"

Any other return value is treated as \"idle\"."
  :type '(function :tag "State normalizer")
  :group 'eat-tmux)

(defcustom eat-tmux-manager-pane-state-alist
  '(("running" . "running")
    ("busy" . "running")
    ("needs-input" . "needs-input")
    ("done" . "needs-input"))
  "Raw-to-normalized pane state mapping used by default normalizer.

Keys are compared case-insensitively after trimming whitespace."
  :type '(alist :key-type string
                :value-type (choice
                             (const :tag "Needs Input" "needs-input")
                             (const :tag "Running" "running")
                             (const :tag "Idle" "idle")))
  :group 'eat-tmux)

(defconst eat-tmux-manager--buffer-name "*eat-tmux-manager*"
  "Name of the `eat-tmux-manager' buffer.")

(defconst eat-tmux-manager--columns
  [("Src" 18 t)
   ("Session" 30 t)
   ("Path" 24 t)
   ("Windows" 0 t)]
  "Tabulated columns used by `eat-tmux-manager-mode'.")

(defvar-local eat-tmux-manager--records (make-hash-table :test #'equal)
  "Map from tabulated row ID to session record plist.")

(defface eat-tmux-manager-window-needs-input-face
  '((t :inherit warning))
  "Face for manager windows waiting for user input."
  :group 'eat-tmux)

(defface eat-tmux-manager-window-running-face
  '((t :inherit font-lock-keyword-face))
  "Face for manager windows with active pane activity."
  :group 'eat-tmux)

(defface eat-tmux-manager-window-idle-face
  '((t :inherit shadow))
  "Face for manager windows that are idle."
  :group 'eat-tmux)

(defconst eat-tmux--id-file-name ".eat-tmux-id"
  "File used for stable session identity when using `id-file' strategy.")

(defun eat-tmux--ensure-tmux ()
  "Ensure tmux executable is available."
  (unless (executable-find "tmux")
    (user-error "`tmux' executable was not found in PATH")))

(defun eat-tmux--ensure-ssh ()
  "Ensure ssh executable is available."
  (unless (executable-find "ssh")
    (user-error "`ssh' executable was not found in PATH")))

(defun eat-tmux--remote-context (directory)
  "Return remote context plist for DIRECTORY, or nil when local."
  (when (file-remote-p directory)
    (pcase eat-tmux-remote-execution
      ('error
       (user-error "Remote projects are disabled (`eat-tmux-remote-execution' is `error')"))
      ('ssh
       (unless (require 'tramp nil t)
         (user-error "TRAMP is required for remote project support"))
       (let* ((vec (tramp-dissect-file-name directory))
              (method (tramp-file-name-method vec))
              (user (tramp-file-name-user vec))
              (host (tramp-file-name-host vec))
              (port (tramp-file-name-port vec))
              (remote-dir (tramp-file-name-localname vec)))
         (unless (member method '("ssh" "sshx" "scp" "scpx"))
           (user-error "Remote method %S is unsupported; use ssh/scp TRAMP methods"
                       method))
         (list :method method
               :user user
               :host host
               :port port
               :directory remote-dir)))
      (_
       (user-error "Invalid `eat-tmux-remote-execution': %S"
                   eat-tmux-remote-execution)))))

(defun eat-tmux--ssh-target (remote)
  "Return user@host target string for REMOTE plist."
  (let ((user (plist-get remote :user))
        (host (plist-get remote :host)))
    (if (and (stringp user) (not (string-empty-p user)))
        (format "%s@%s" user host)
      host)))

(defun eat-tmux--ssh-shell-command (remote remote-command &optional force-tty)
  "Return local shell command to run REMOTE-COMMAND over ssh.

When FORCE-TTY is non-nil, pass `-tt' to ssh."
  (let* ((q #'shell-quote-argument)
         (target (eat-tmux--ssh-target remote))
         (port (plist-get remote :port))
         (parts (list "exec" "ssh")))
    (when force-tty
      (setq parts (append parts (list "-tt"))))
    (when (and (stringp port) (not (string-empty-p port)))
      (setq parts (append parts (list "-p" (funcall q port)))))
    (setq parts (append parts
                        (list (funcall q target)
                              (funcall q remote-command))))
    (mapconcat #'identity parts " ")))

(defun eat-tmux--shell-command (argv)
  "Return a shell-safe command string from ARGV."
  (mapconcat #'shell-quote-argument argv " "))

(defun eat-tmux--ssh-command-output-string (remote remote-command &optional require-success)
  "Run REMOTE-COMMAND via ssh for REMOTE and return raw output string.

When REQUIRE-SUCCESS is non-nil, signal a user error if command fails."
  (with-temp-buffer
    (let* ((port (plist-get remote :port))
           (args (append (when (and (stringp port) (not (string-empty-p port)))
                           (list "-p" port))
                         (list (eat-tmux--ssh-target remote)
                               remote-command)))
           (status (apply #'process-file "ssh" nil t nil args)))
      (if (zerop status)
          (buffer-string)
        (when require-success
          (user-error "Remote command failed (%d): %s"
                      status
                      (string-trim (buffer-string))))))))

(defun eat-tmux--ssh-command-output-lines (remote remote-command &optional require-success)
  "Run REMOTE-COMMAND via ssh for REMOTE and return output lines or nil.

When REQUIRE-SUCCESS is non-nil, signal a user error if command fails."
  (when-let* ((output (eat-tmux--ssh-command-output-string
                       remote remote-command require-success)))
    (split-string output "\n" t)))

(defun eat-tmux--remote-tramp-default-directory (remote)
  "Return a TRAMP `default-directory' for REMOTE."
  (let* ((dir (or (plist-get remote :directory) "~"))
         (raw (if (and (stringp dir) (not (string-empty-p dir)))
                  dir
                "~")))
    (file-name-as-directory
     (eat-tmux--remote-path->tramp remote raw))))

(defun eat-tmux--tramp-command-output-string (remote remote-command &optional require-success)
  "Run REMOTE-COMMAND for REMOTE via TRAMP and return raw output string.

When REQUIRE-SUCCESS is non-nil, signal a user error if command fails."
  (with-temp-buffer
    (let ((default-directory (eat-tmux--remote-tramp-default-directory remote)))
      (condition-case err
          (let ((status (process-file "sh" nil t nil "-lc" remote-command)))
            (if (zerop status)
                (buffer-string)
              (when require-success
                (user-error "Remote command failed (%d): %s"
                            status
                            (string-trim (buffer-string))))))
        (file-error
         (when require-success
           (user-error "Remote command failed: %s"
                       (error-message-string err))))))))

(defun eat-tmux--remote-command-output-string (remote remote-command &optional require-success)
  "Run REMOTE-COMMAND for REMOTE via ssh, falling back to TRAMP.

When REQUIRE-SUCCESS is non-nil, signal a user error if both transports fail."
  (or (eat-tmux--ssh-command-output-string remote remote-command)
      (eat-tmux--tramp-command-output-string remote remote-command require-success)))

(defun eat-tmux--remote-command-output-lines (remote remote-command &optional require-success)
  "Run REMOTE-COMMAND for REMOTE and return output lines, or nil.

When REQUIRE-SUCCESS is non-nil, signal a user error if command fails."
  (when-let* ((output (eat-tmux--remote-command-output-string
                       remote remote-command require-success)))
    (split-string output "\n" t)))

(defun eat-tmux--resolve-remote-directory (remote directory)
  "Return DIRECTORY for REMOTE as an absolute path when possible."
  (cond
   ((not (stringp directory))
    directory)
   ((not (string-prefix-p "~" directory))
    directory)
   ((and (not (string= directory "~"))
         (not (string-prefix-p "~/" directory)))
   directory)
   (t
    (let ((home (car (eat-tmux--remote-command-output-lines
                      remote
                      "printf '%s\\n' \"$HOME\""))))
      (if (and (stringp home) (string-prefix-p "/" home))
          (if (or (string= directory "~")
                  (string= directory "~/"))
              home
            (concat (file-name-as-directory home)
                    (string-remove-prefix "~/" directory)))
        directory)))))

(defun eat-tmux--project-context ()
  "Return project context plist with root and session base."
  (let* ((project (project-current t))
         (root (project-root project)))
    (list :project project
          :root root
          :session-base (eat-tmux--session-base-name project)
          :remote (eat-tmux--remote-context root))))

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
  (if (and (stringp session-name)
           (string-match "\\`\\(.*\\)\\(?:--view\\|:view\\)\\([0-9]+\\)\\'" session-name))
      (list (match-string 1 session-name)
            (string-to-number (match-string 2 session-name)))
    (list session-name 1)))

(defun eat-tmux--session-context (session-name)
  "Return plist with tmux context derived from SESSION-NAME."
  (pcase-let ((`(,session-base ,view-index)
               (eat-tmux--session-name-components session-name)))
    (list :session-name session-name
          :session-base session-base
          :view-index view-index)))

(defun eat-tmux--path-session-base (project)
  "Return path-based tmux session base name for PROJECT."
  (let* ((project-root (project-root project))
         ;; Avoid `file-truename' on TRAMP roots, which can block on
         ;; remote connection setup when we only need a stable identifier.
         (remote-vec (and (file-remote-p project-root)
                          (require 'tramp nil t)
                          (tramp-dissect-file-name project-root)))
         (root (cond
                (remote-vec
                 (directory-file-name
                  (or (tramp-file-name-localname remote-vec)
                      project-root)))
                ((file-remote-p project-root)
                 (directory-file-name project-root))
                (t
                 (directory-file-name (file-truename project-root)))))
         (hash-input (if remote-vec
                         (format "%s:%s:%s:%s:%s"
                                 (or (tramp-file-name-method remote-vec) "")
                                 (or (tramp-file-name-user remote-vec) "")
                                 (or (tramp-file-name-host remote-vec) "")
                                 (or (tramp-file-name-port remote-vec) "")
                                 root)
                       root))
         (base (file-name-nondirectory root))
         (hash (substring (secure-hash 'sha1 hash-input) 0 8)))
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

(defun eat-tmux--buffer-mode-name (mode session-base &optional remote)
  "Return MODE string used by `project-prefixed-buffer-name'.

Include a short SESSION-BASE fingerprint to avoid collisions between
projects with the same basename.  For REMOTE projects include host too."
  (let ((suffix (substring (secure-hash 'sha1 session-base) 0 6)))
    (if remote
        (format "%s@%s-%s"
                mode
                (or (plist-get remote :host) "remote")
                suffix)
      (format "%s-%s" mode suffix))))

(defun eat-tmux--remote-key (remote)
  "Return normalized comparison key for REMOTE."
  (when remote
    (list (plist-get remote :method)
          (plist-get remote :user)
          (plist-get remote :host)
          (plist-get remote :port))))

(defun eat-tmux--same-remote-p (a b)
  "Return non-nil when remote contexts A and B are equivalent."
  (equal (eat-tmux--remote-key a)
         (eat-tmux--remote-key b)))

(defun eat-tmux--remote-tramp-prefix (remote)
  "Return TRAMP prefix for REMOTE context."
  (let ((user (plist-get remote :user))
        (host (or (plist-get remote :host) ""))
        (port (plist-get remote :port)))
    (format "/ssh:%s%s%s:"
            (if (and (stringp user) (not (string-empty-p user)))
                (concat user "@")
              "")
            host
            (if (and (stringp port) (not (string-empty-p port)))
                (format "#%s" port)
              ""))))

(defun eat-tmux--remote-path->tramp (remote path)
  "Convert REMOTE tmux PATH to a TRAMP path."
  (if (or (not (stringp path)) (string-empty-p path))
      (eat-tmux--remote-tramp-prefix remote)
    (let ((prefix (eat-tmux--remote-tramp-prefix remote)))
      (if (file-name-absolute-p path)
          (concat prefix path)
        (concat prefix "/" path)))))

(defun eat-tmux--buffer-session-name ()
  "Return current buffer's tmux session name."
  (or eat-tmux--session-name
      (and (stringp eat-tmux--session-base)
           (integerp eat-tmux--view-index)
           (eat-tmux--view-session-name eat-tmux--session-base
                                        eat-tmux--view-index))))

(defun eat-tmux--view-buffer-name (base-name view-index)
  "Return buffer name for BASE-NAME and VIEW-INDEX."
  (if (= view-index 1)
      base-name
    (format "%s<%d>" base-name view-index)))

(defun eat-tmux--stored-base-buffer-name ()
  "Return current buffer's base tmux buffer name."
  (or eat-tmux--base-buffer-name
      (let ((name (buffer-name)))
        (if (and (integerp eat-tmux--view-index)
                 (> eat-tmux--view-index 1)
                 (string-match
                  (format "\\`\\(.*\\)<%d>\\'" eat-tmux--view-index)
                  name))
            (match-string 1 name)
          name))))

(defun eat-tmux--buffer-matches-context-p (buffer session-base view-index remote
                                                  &optional session-name)
  "Return non-nil when BUFFER matches tmux context identifiers.

BUFFER must match SESSION-BASE, VIEW-INDEX, and REMOTE.  When
SESSION-NAME is non-nil, BUFFER must also match that exact session."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'eat-mode)
              (stringp eat-tmux--session-base)
              (string= eat-tmux--session-base session-base)
              (integerp eat-tmux--view-index)
              (= eat-tmux--view-index view-index)
              (or (null session-name)
                  (string= (or (eat-tmux--buffer-session-name) "")
                           session-name))
              (eat-tmux--same-remote-p eat-tmux--remote-context remote)))))

(defun eat-tmux--buffer-matches-session-p (buffer session-name remote)
  "Return non-nil when BUFFER matches SESSION-NAME and REMOTE."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (derived-mode-p 'eat-mode)
              (string= (or (eat-tmux--buffer-session-name) "")
                       (or session-name ""))
              (eat-tmux--same-remote-p eat-tmux--remote-context remote)))))

(defun eat-tmux--find-session-buffer (session-name remote)
  "Return existing Eat buffer for SESSION-NAME and REMOTE."
  (catch 'found
    (dolist (buffer (buffer-list))
      (when (eat-tmux--buffer-matches-session-p buffer session-name remote)
        (throw 'found buffer)))
    nil))

(defun eat-tmux--find-context-buffer (session-base view-index remote
                                                  &optional session-name)
  "Return existing Eat buffer for SESSION-BASE VIEW-INDEX and REMOTE.

When SESSION-NAME is non-nil, require an exact tmux session match too."
  (catch 'found
    (dolist (buffer (buffer-list))
      (when (eat-tmux--buffer-matches-context-p
             buffer session-base view-index remote session-name)
        (throw 'found buffer)))
    nil))

(defun eat-tmux--find-context-base-buffer-name (session-base remote)
  "Return existing base buffer name for SESSION-BASE and REMOTE, or nil."
  (catch 'found
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and (derived-mode-p 'eat-mode)
                        (stringp eat-tmux--session-base)
                        (string= eat-tmux--session-base session-base)
                        (eat-tmux--same-remote-p eat-tmux--remote-context remote))))
        (throw 'found
               (with-current-buffer buffer
                 (eat-tmux--stored-base-buffer-name)))))
    nil))

(defun eat-tmux--resolve-buffer-names (project-root session-base view-index remote
                                                    &optional session-name)
  "Return plist with `:base' and `:name' for tmux context buffers.

When SESSION-NAME is non-nil, buffer conflict checks require exact
session-name matches for this view."
  (let* ((preferred-base (let ((default-directory project-root))
                           (project-prefixed-buffer-name "tmux")))
         (existing-buffer
          (eat-tmux--find-context-buffer session-base
                                         view-index
                                         remote
                                         session-name)))
    (if existing-buffer
        (with-current-buffer existing-buffer
          (list :base (eat-tmux--stored-base-buffer-name)
                :name (buffer-name existing-buffer)))
      (let* ((existing-base
              (eat-tmux--find-context-base-buffer-name session-base remote))
             (base-name (or existing-base preferred-base))
             (base-index nil))
        (unless existing-base
          (while (or (get-buffer base-name)
                     (let ((buffer (get-buffer
                                    (eat-tmux--view-buffer-name
                                     base-name view-index))))
                       (and buffer
                            (not (eat-tmux--buffer-matches-context-p
                                  buffer
                                  session-base
                                  view-index
                                  remote
                                  session-name)))))
            (setq base-index (if base-index (1+ base-index) 2)
                  base-name (eat-tmux--indexed-base-buffer-name
                             preferred-base base-index))))
        (let ((name (eat-tmux--view-buffer-name base-name view-index)))
          (while (let ((buffer (get-buffer name)))
                   (and buffer
                        (not (eat-tmux--buffer-matches-context-p
                              buffer
                              session-base
                              view-index
                              remote
                              session-name))))
            (setq base-index (if base-index (1+ base-index) 2)
                  base-name (eat-tmux--indexed-base-buffer-name
                             preferred-base base-index))
            (setq name (eat-tmux--view-buffer-name base-name view-index)))
          (list :base base-name :name name))))))

(defun eat-tmux--indexed-base-buffer-name (preferred-base index)
  "Return a collision-disambiguated base name from PREFERRED-BASE and INDEX.

Unlike view naming, this uses \"#N\" instead of \"<N>\" so it cannot be
confused with tmux view indices."
  (if (and (string-prefix-p "*" preferred-base)
           (string-suffix-p "*" preferred-base))
      (format "%s#%d*" (substring preferred-base 0 -1) index)
    (format "%s#%d" preferred-base index)))

(defun eat-tmux--tmux-command-lines (remote &rest args)
  "Run tmux ARGS locally or on REMOTE and return output lines, or nil."
  (if remote
      (eat-tmux--remote-command-output-lines
       remote
       (eat-tmux--shell-command (cons "tmux" args)))
    (condition-case nil
        (apply #'process-lines "tmux" args)
      (error nil))))

(defun eat-tmux--tmux-command-require-success (remote &rest args)
  "Run tmux ARGS locally or on REMOTE, signaling on failure."
  (if remote
      (progn
        (eat-tmux--remote-command-output-string
         remote
         (eat-tmux--shell-command (cons "tmux" args))
         t)
        t)
    (with-temp-buffer
      (let ((status (apply #'process-file "tmux" nil t nil args)))
        (if (zerop status)
            t
          (user-error "Tmux command failed (%d): tmux %s%s"
                      status
                      (mapconcat #'identity args " ")
                      (let ((msg (string-trim (buffer-string))))
                        (if (string-empty-p msg)
                            ""
                          (format " (%s)" msg)))))))))

(defun eat-tmux--list-sessions (&optional remote)
  "Return tmux session names locally or on REMOTE."
  (eat-tmux--tmux-command-lines remote
                                "list-sessions" "-F" "#{session_name}"))

(defun eat-tmux--view-session-name (session-base view-index)
  "Return tmux session name for SESSION-BASE and VIEW-INDEX."
  (if (= view-index 1)
      session-base
    (format "%s--view%d" session-base view-index)))

(defun eat-tmux--next-view-index (session-base &optional remote)
  "Return the next free view index for SESSION-BASE.

SESSION-BASE itself counts as view 1."
  (let ((re (format "\\`%s\\(?:--view\\|:view\\)\\([0-9]+\\)\\'"
                    (regexp-quote session-base)))
        (max-view 1))
    (dolist (name (eat-tmux--list-sessions remote))
      (cond
       ((string= name session-base)
        (setq max-view (max max-view 1)))
       ((string-match re name)
        (setq max-view (max max-view
                            (string-to-number (match-string 1 name)))))))
    (1+ max-view)))

(defun eat-tmux--command (session-base session-name view-index directory)
  "Return shell command string for SESSION-NAME in DIRECTORY.

SESSION-BASE and VIEW-INDEX are used when SESSION-NAME represents a
derived view (`--viewN' or `:viewN')."
  (let ((q #'shell-quote-argument))
    (mapconcat
     #'identity
     (delq nil
           (list
            (format "cd %s || exit 1" (funcall q directory))
            (format "tmux has-session -t %s 2>/dev/null || tmux new-session -Ad -s %s -c %s"
                    (funcall q session-base)
                    (funcall q session-base)
                    (funcall q directory))
            (unless (= view-index 1)
              (format "tmux has-session -t %s 2>/dev/null || tmux new-session -Ad -t %s -s %s -c %s"
                      (funcall q session-name)
                      (funcall q session-base)
                      (funcall q session-name)
                      (funcall q directory)))
            (format "if [ -n \"$TMUX\" ]; then tmux switch-client -t %s; else exec tmux attach-session -t %s -c %s; fi"
                    (funcall q session-name)
                    (funcall q session-name)
                    (funcall q directory))))
     "; ")))

(defun eat-tmux--disable-kill-query (buffer)
  "Make killing BUFFER detach tmux without a confirmation prompt."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local kill-buffer-query-functions
                  (remove #'process-kill-buffer-query-function
                          kill-buffer-query-functions))
      (when-let* ((proc (get-buffer-process buffer)))
        (set-process-query-on-exit-flag proc nil)))))

(defun eat-tmux--session-default-directory (directory remote)
  "Return Emacs `default-directory' for DIRECTORY and REMOTE."
  (if remote
      (if (file-remote-p directory)
          directory
        (eat-tmux--remote-path->tramp remote directory))
    (expand-file-name directory)))

(defun eat-tmux--tmux-directory (directory remote)
  "Return tmux-side working directory from DIRECTORY and REMOTE."
  (if remote
      (let ((remote-directory (if (file-remote-p directory)
                                  (plist-get (eat-tmux--remote-context directory) :directory)
                                directory)))
        (eat-tmux--resolve-remote-directory remote remote-directory))
    (expand-file-name directory)))

(defun eat-tmux--open-session (directory session-name &optional remote)
  "Open tmux SESSION-NAME in DIRECTORY.

When REMOTE is non-nil, run tmux commands through that remote context.
Reuse an existing Eat buffer for this session/context when available;
otherwise create a fresh buffer and attach."
  (unless (require 'eat nil t)
    (user-error "Package `eat' is not available"))
  (unless (and (stringp session-name)
               (not (string-empty-p (string-trim session-name))))
    (user-error "Session name must be a non-empty string"))
  (let* ((resolved-directory (or directory default-directory))
         (remote-context (or remote
                             (eat-tmux--remote-context resolved-directory)))
         (session-context (eat-tmux--session-context session-name))
         (session-base (plist-get session-context :session-base))
         (view-index (plist-get session-context :view-index))
         (local-directory (eat-tmux--session-default-directory
                           resolved-directory
                           remote-context))
         (tmux-directory (eat-tmux--tmux-directory
                          resolved-directory
                          remote-context))
         (existing (eat-tmux--find-session-buffer session-name remote-context)))
    (when (< view-index 1)
      (user-error "View index must be >= 1"))
    (if remote-context
        (eat-tmux--ensure-ssh)
      (eat-tmux--ensure-tmux))
    (if (buffer-live-p existing)
        (progn
          (pop-to-buffer existing)
          existing)
      (let* ((buffer-info (eat-tmux--resolve-buffer-names local-directory
                                                          session-base
                                                          view-index
                                                          remote-context
                                                          session-name))
             (base-buffer-name (plist-get buffer-info :base))
             (buffer-name (plist-get buffer-info :name))
             ;; For remote contexts, use a local default-directory so
             ;; eat's make-process does not go through TRAMP (which
             ;; would create a pipe instead of a real PTY, causing
             ;; tmux to fail with "not a terminal").  The SSH wrapper
             ;; with -tt handles PTY allocation on the remote side.
             ;; For local contexts, use local-directory rather than
             ;; default-directory which may still be a TRAMP path
             ;; from the calling buffer (e.g. after C-x p p from a
             ;; remote project).
             (launch-directory (if remote-context
                                   (expand-file-name "~")
                                 local-directory))
             (tmux-command (eat-tmux--command session-base
                                              session-name
                                              view-index
                                              tmux-directory))
             (command (if remote-context
                          (eat-tmux--ssh-shell-command
                           remote-context tmux-command t)
                        tmux-command))
             (eat-buffer-name buffer-name)
             (eat-arg nil)
             (buffer (let ((default-directory launch-directory))
                       (eat command eat-arg))))
        (with-current-buffer buffer
          (setq-local eat-tmux--session-base session-base
                      eat-tmux--view-index view-index
                      eat-tmux--session-name session-name
                      eat-tmux--remote-context remote-context
                      eat-tmux--base-buffer-name base-buffer-name)
          (setq-local default-directory local-directory)
          (eat-tmux-mode 1))
        (eat-tmux--disable-kill-query buffer)
        (pop-to-buffer buffer)
        buffer))))

(defun eat-tmux--read-open-args ()
  "Return interactive argument list for `eat-tmux-open'."
  (let* ((directory (read-directory-name "Directory: " default-directory nil t))
         (remote (eat-tmux--remote-context directory))
         (sessions (eat-tmux--list-sessions remote))
         (default-session (or (eat-tmux--buffer-session-name)
                              (car sessions)))
         (session-name (completing-read "Session: "
                                        sessions
                                        nil
                                        nil
                                        nil
                                        nil
                                        default-session)))
    (list directory session-name remote)))

;;;###autoload
(defun eat-tmux-open (directory session-name &optional remote)
  "Open or create tmux SESSION-NAME from DIRECTORY in an Eat buffer.

If a matching Eat buffer already exists, jump to it.  Otherwise open a
fresh buffer and attach.  If SESSION-NAME does not exist yet, it is created."
  (interactive (eat-tmux--read-open-args))
  (eat-tmux--open-session directory session-name remote))

(defun eat-tmux--capture-pane-text (pane-id &optional remote)
  "Capture all available text from tmux PANE-ID.

When REMOTE is non-nil, run on the remote tmux server."
  (if remote
      (let ((output (eat-tmux--remote-command-output-string
                     remote
                     (eat-tmux--shell-command
                      (list "tmux" "capture-pane" "-p" "-J" "-S" "-"
                            "-t" pane-id))
                     t)))
        (or output ""))
    (with-temp-buffer
      (let ((status (process-file "tmux" nil t nil
                                  "capture-pane" "-p" "-J" "-S" "-"
                                  "-t" pane-id)))
        (if (zerop status)
            (buffer-string)
          (user-error "Could not capture tmux pane %s: %s"
                      pane-id (string-trim (buffer-string))))))))

(defun eat-tmux--current-pane-id (session-target &optional remote)
  "Return ID of active pane in current tmux window of SESSION-TARGET.

When REMOTE is non-nil, run on the remote tmux server."
  (let* ((output
          (if remote
              (or (eat-tmux--remote-command-output-string
                   remote
                   (eat-tmux--shell-command
                    (list "tmux" "display-message" "-p"
                          "-t" session-target "#{pane_id}"))
                   t)
                  "")
            (with-temp-buffer
              (let ((status (process-file "tmux" nil t nil
                                          "display-message"
                                          "-p"
                                          "-t" session-target
                                          "#{pane_id}")))
                (unless (zerop status)
                  (user-error "Could not get current tmux pane for %s: %s"
                              session-target (string-trim (buffer-string)))))
              (buffer-string))))
         (pane-id (string-trim output)))
    (if (string-empty-p pane-id)
        (user-error "No active pane found for %s" session-target)
      pane-id)))

(defun eat-tmux--capture-context-from-current-buffer ()
  "Return live tmux context from current Eat buffer, or nil."
  (when (derived-mode-p 'eat-mode)
    (if eat-tmux--remote-context
        (when (and (stringp eat-tmux--session-base)
                   (integerp eat-tmux--view-index))
          (let* ((session-base eat-tmux--session-base)
                 (view-index eat-tmux--view-index)
                 (session-name (or eat-tmux--session-name
                                   (eat-tmux--view-session-name
                                    session-base view-index)))
                 (pane-id (eat-tmux--current-pane-id session-name
                                                     eat-tmux--remote-context)))
            (list :session-base session-base
                  :view-index view-index
                  :session-name session-name
                  :pane-id pane-id
                  :remote eat-tmux--remote-context)))
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
                  (eat-tmux--current-pane-id session-name)))
          (list :session-base session-base
                :view-index view-index
                :session-name session-name
                :pane-id pane-id
                :remote nil))))))

(defun eat-tmux--fallback-capture-context (&optional remote)
  "Return capture context based on project/session metadata.

When REMOTE is non-nil, run capture queries against remote tmux."
  (let* ((context (eat-tmux--project-context))
         (effective-remote (or remote (plist-get context :remote)))
         (session-base (plist-get context :session-base))
         (view-index (eat-tmux--current-view-index session-base))
         (session-name (eat-tmux--view-session-name session-base view-index))
         (pane-id (eat-tmux--current-pane-id session-name
                                             effective-remote)))
    (list :session-base session-base
          :view-index view-index
          :session-name session-name
          :pane-id pane-id
          :remote effective-remote)))

(defun eat-tmux--capture-buffer-name (session-base view-index &optional remote)
  "Return capture buffer name for SESSION-BASE and VIEW-INDEX.

When REMOTE is non-nil, include remote discriminator in project-prefixed
buffer names."
  (let ((base-name (or (and (require 'project nil t)
                            (ignore-errors
                              (project-prefixed-buffer-name
                               (eat-tmux--buffer-mode-name "tmux-capture"
                                                           session-base
                                                           remote))))
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
  (let* ((capture-context
          (or (eat-tmux--capture-context-from-current-buffer)
              (progn
                (require 'project)
                (eat-tmux--fallback-capture-context
                 (and (derived-mode-p 'eat-mode)
                      eat-tmux--remote-context)))))
         (session-base (plist-get capture-context :session-base))
         (view-index (plist-get capture-context :view-index))
         (pane-id (plist-get capture-context :pane-id))
         (remote (plist-get capture-context :remote))
         (_ (if remote
                (eat-tmux--ensure-ssh)
              (eat-tmux--ensure-tmux)))
         (text (eat-tmux--capture-pane-text pane-id remote))
         (buffer-name (eat-tmux--capture-buffer-name session-base
                                                     view-index
                                                     remote))
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
(defun eat-tmux-project (&optional arg)
  "Open a project-local tmux view in an Eat buffer.

With no prefix ARG, attach/create view 1.
With numeric prefix ARG N, attach/create view N.
With non-numeric prefix ARG (for example `C-u'), create the next
available view index."
  (interactive "P")
  (require 'project)
  (let* ((context (eat-tmux--project-context))
         (project-root (plist-get context :root))
         (remote (plist-get context :remote))
         (session-base (plist-get context :session-base))
         (view-index (cond
                      ((numberp arg)
                       (prefix-numeric-value arg))
                      (arg
                       (eat-tmux--next-view-index session-base remote))
                      (t 1)))
         (session-name (eat-tmux--view-session-name session-base view-index)))
    (eat-tmux--open-session project-root session-name remote)))

(defun eat-tmux--collect-remote-contexts ()
  "Return unique remote contexts from live `eat-tmux-mode' buffers."
  (let ((seen (make-hash-table :test #'equal))
        remotes)
    (when eat-tmux-manager-include-remote
      (dolist (buffer (buffer-list))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (and (bound-and-true-p eat-tmux-mode)
                       eat-tmux--remote-context)
              (let* ((remote eat-tmux--remote-context)
                     (key (eat-tmux--remote-key remote)))
                (unless (gethash key seen)
                  (puthash key t seen)
                  (push remote remotes))))))))
    (nreverse remotes)))

(defun eat-tmux-manager--parse-pane-state-env-lines (lines)
  "Parse pane state metadata from tmux environment LINES.

Returns a hash table mapping pane IDs to raw state strings according to
`eat-tmux-manager-pane-state-env-regexp'."
  (let ((states (make-hash-table :test #'equal)))
    (condition-case err
        (dolist (line (or lines '()))
          (when (string-match eat-tmux-manager-pane-state-env-regexp line)
            (puthash (match-string 1 line) (match-string 2 line) states)))
      (invalid-regexp
       (message "eat-tmux: invalid pane state regexp %S (%s)"
                eat-tmux-manager-pane-state-env-regexp
                (error-message-string err))))
    states))

(defun eat-tmux-manager-pane-state-provider-tmux-environment (remote)
  "Default pane state provider for `eat-tmux-manager'.

Read `tmux show-environment -g' for REMOTE and parse it using
`eat-tmux-manager-pane-state-env-regexp'.  This works with
tmux-agent-indicator-style variables by default."
  (eat-tmux-manager--parse-pane-state-env-lines
   (eat-tmux--tmux-command-lines remote "show-environment" "-g")))

(defun eat-tmux-manager-normalize-pane-state (raw-state)
  "Normalize RAW-STATE to a manager window state string.

Uses `eat-tmux-manager-pane-state-alist'."
  (let* ((key (downcase (string-trim (or raw-state ""))))
         (mapped (cdr (assoc-string key eat-tmux-manager-pane-state-alist t))))
    (or mapped "idle")))

(defun eat-tmux-manager--normalized-pane-state (raw-state)
  "Return normalized pane state for RAW-STATE using custom normalizer."
  (let ((state (if (functionp eat-tmux-manager-pane-state-normalizer)
                   (funcall eat-tmux-manager-pane-state-normalizer raw-state)
                 "idle")))
    (if (member state '("needs-input" "running" "idle"))
        state
      "idle")))

(defun eat-tmux-manager--pane-state-map (&optional remote)
  "Return pane state map hash table for REMOTE, or an empty map."
  (let ((empty (make-hash-table :test #'equal)))
    (if (not (functionp eat-tmux-manager-pane-state-provider))
        empty
      (condition-case err
          (let ((result (funcall eat-tmux-manager-pane-state-provider remote)))
            (cond
             ((hash-table-p result) result)
             ((null result) empty)
             (t
              (message "eat-tmux: pane state provider returned %S, expected hash table or nil"
                       (type-of result))
              empty)))
        (error
         (message "eat-tmux: pane state provider failed: %s"
                  (error-message-string err))
         empty)))))

;; Backward-compatible aliases for older internal names.
(defalias 'eat-tmux-manager--parse-agent-indicator-env-lines
  #'eat-tmux-manager--parse-pane-state-env-lines)

(defalias 'eat-tmux-manager--normalize-indicator-state
  #'eat-tmux-manager--normalized-pane-state)

(defun eat-tmux-manager--state-rank (state)
  "Return manager sort rank for STATE."
  (pcase state
    ("needs-input" 3)
    ("running" 2)
    (_ 1)))

(defun eat-tmux-manager--merge-state (current candidate)
  "Merge CURRENT and CANDIDATE window state."
  (if (> (eat-tmux-manager--state-rank candidate)
         (eat-tmux-manager--state-rank current))
      candidate
    current))

(defun eat-tmux-manager--window-face (state)
  "Return face symbol used for window STATE."
  (pcase state
    ("needs-input" 'eat-tmux-manager-window-needs-input-face)
    ("running" 'eat-tmux-manager-window-running-face)
    (_ 'eat-tmux-manager-window-idle-face)))

(defun eat-tmux-manager--window-index< (a b)
  "Return non-nil when window record A should sort before B."
  (let* ((a-idx (or (plist-get a :index) "0"))
         (b-idx (or (plist-get b :index) "0"))
         (a-num (and (string-match-p "\\`[0-9]+\\'" a-idx)
                     (string-to-number a-idx)))
         (b-num (and (string-match-p "\\`[0-9]+\\'" b-idx)
                     (string-to-number b-idx))))
    (cond
     ((and a-num b-num) (< a-num b-num))
     (a-num t)
     (b-num nil)
     (t (string< a-idx b-idx)))))

(defun eat-tmux-manager--window-label (window)
  "Return propertized label for WINDOW plist."
  (let* ((index (or (plist-get window :index) ""))
         (name (or (plist-get window :name) ""))
         (state (or (plist-get window :state) "idle"))
         (active (plist-get window :active))
         (label (format "%s:%s%s" index name (if active "*" ""))))
    (propertize label
                'face (eat-tmux-manager--window-face state)
                'help-echo (format "Window state: %s" state))))

(defun eat-tmux-manager--windows-string (windows)
  "Return propertized window summary string for WINDOWS."
  (if windows
      (mapconcat #'eat-tmux-manager--window-label windows " ")
    ""))

(defun eat-tmux-manager--display-path (path)
  "Return manager display string for PATH.

Long paths are truncated from the left so the rightmost, most
informative path suffix remains visible."
  (let* ((raw (or path ""))
         (short (abbreviate-file-name raw))
         (width (max 4 eat-tmux-manager-path-column-width))
         (len (length short)))
    (if (<= len width)
        short
      (let* ((tail (substring short
                              (max 0 (- len (1- width)))))
             (slash (string-match "/" tail)))
        (when (and slash (> slash 0))
          (setq tail (substring tail slash)))
        (concat "…" tail)))))

(defun eat-tmux--session-window-table (&optional remote)
  "Return hash table of session metadata for local tmux or REMOTE.

Each value is a plist with keys:
- `:path': session working path (active pane preferred)
- `:windows': sorted list of window plists with `:index', `:name', `:state'."
  (let* ((state-map (eat-tmux-manager--pane-state-map remote))
         (table (make-hash-table :test #'equal)))
    (dolist (line (eat-tmux--tmux-command-lines
                   remote
                   "list-panes" "-a" "-F"
                   "#{session_name}\t#{window_index}\t#{window_name}\t#{pane_id}\t#{pane_active}\t#{pane_current_path}\t#{window_active}"))
      (pcase-let* ((`(,session ,window-index ,window-name ,pane-id ,pane-active ,pane-path ,window-active . ,_)
                    (split-string line "\t"))
                   (entry (or (and (stringp session) (gethash session table))
                              (list :path nil
                                    :windows (make-hash-table :test #'equal)))))
        (when (and (stringp session) (not (string-empty-p session)))
          (when (and (stringp pane-path) (not (string-empty-p pane-path)))
            (when (or (null (plist-get entry :path))
                      (equal pane-active "1"))
              (setq entry (plist-put entry :path pane-path))))
          (when (and (stringp window-index) (not (string-empty-p window-index)))
            (let* ((windows (plist-get entry :windows))
                   (window (or (gethash window-index windows)
                               (list :index window-index :name "" :state "idle")))
                   (pane-state (eat-tmux-manager--normalized-pane-state
                                (and (stringp pane-id)
                                     (gethash pane-id state-map))))
                   (window-state (eat-tmux-manager--merge-state
                                  (or (plist-get window :state) "idle")
                                  pane-state)))
              (setq window (plist-put window :name (or window-name "")))
              (setq window (plist-put window :state window-state))
              (when (equal window-active "1")
                (setq window (plist-put window :active t)))
              (puthash window-index window windows)))
          (puthash session entry table))))
    (maphash
     (lambda (session entry)
       (let (windows)
         (maphash
          (lambda (_key window)
            (push window windows))
          (plist-get entry :windows))
         (puthash session
                  (list :path (or (plist-get entry :path) "")
                        :windows (sort windows #'eat-tmux-manager--window-index<))
                  table)))
     table)
    table))

(defun eat-tmux-manager--collect-session-records-for-context (remote)
  "Collect tmux session records for REMOTE context or local tmux."
  (let* ((session-table (eat-tmux--session-window-table remote))
         (source (if-let* ((host (and remote (plist-get remote :host)))
                           ((not (string-empty-p host))))
                     (format "tmux@%s" host)
                   "tmux"))
         (remote-key (and remote (eat-tmux--remote-key remote)))
         records)
    (dolist (session (eat-tmux--list-sessions remote))
      (let* ((session-info (or (gethash session session-table) (list :path "" :windows nil)))
             (tmux-path (or (plist-get session-info :path) ""))
             (windows (plist-get session-info :windows))
             (windows-text (eat-tmux-manager--windows-string windows))
             (display-path (if (and remote (not (string-empty-p tmux-path)))
                               (eat-tmux--remote-path->tramp remote tmux-path)
                             tmux-path))
             (display-path-short (eat-tmux-manager--display-path display-path))
             (id (if remote
                     (format "remote:%s:%s" remote-key session)
                   (format "local:%s" session))))
        (push (list :id id
                    :source source
                    :session session
                    :windows windows
                    :windows-text windows-text
                    :path-display display-path-short
                    :path display-path
                    :tmux-path tmux-path
                    :remote remote)
              records)))
    (nreverse records)))

(defun eat-tmux-manager--collect-session-records ()
  "Collect local and remote session records for `eat-tmux-manager'."
  (let ((records (eat-tmux-manager--collect-session-records-for-context nil)))
    (dolist (remote (eat-tmux--collect-remote-contexts))
      (setq records
            (append records
                    (eat-tmux-manager--collect-session-records-for-context
                     remote))))
    records))

(defun eat-tmux-manager--record->entry (record)
  "Return tabulated-list row vector for manager RECORD."
  (vector
   (or (plist-get record :source) "")
   (or (plist-get record :session) "")
   (or (plist-get record :path-display)
       (plist-get record :path)
       "")
   (or (plist-get record :windows-text) "")))

(defun eat-tmux-manager--current-record ()
  "Return session manager record at point."
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No row at point"))
    (or (gethash id eat-tmux-manager--records)
        (user-error "No record for %s" id))))

(defun eat-tmux-manager-refresh ()
  "Refresh session manager rows."
  (interactive)
  (let ((records (eat-tmux-manager--collect-session-records))
        entries)
    (clrhash eat-tmux-manager--records)
    (dolist (record records)
      (let ((id (plist-get record :id)))
        (puthash id record eat-tmux-manager--records)
        (push (list id (eat-tmux-manager--record->entry record)) entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)
    (let ((count (length tabulated-list-entries)))
      (message "eat-tmux-manager: %d session%s"
               count
               (if (= count 1) "" "s")))))

(defun eat-tmux-manager-visit ()
  "Visit tmux session row at point."
  (interactive)
  (let* ((record (eat-tmux-manager--current-record))
         (session (plist-get record :session))
         (remote (plist-get record :remote))
         (tmux-path (plist-get record :tmux-path))
         (directory (if remote
                        (or (and (stringp tmux-path)
                                 (not (string-empty-p tmux-path))
                                 tmux-path)
                            (plist-get remote :directory)
                            "~")
                      (or (and (stringp tmux-path)
                               (not (string-empty-p tmux-path))
                               tmux-path)
                          default-directory))))
    (unless (and (stringp session) (not (string-empty-p session)))
      (user-error "Missing tmux session in row"))
    (eat-tmux--open-session directory session remote)))

(defun eat-tmux-manager-open-directory ()
  "Open selected session directory in `dired'."
  (interactive)
  (let* ((record (eat-tmux-manager--current-record))
         (path (plist-get record :path)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "No path available for this session"))
    (dired path)))

(defun eat-tmux-manager-new ()
  "Prompt for directory/session and open via `eat-tmux-open'."
  (interactive)
  (let* ((seed (and (derived-mode-p 'eat-tmux-manager-mode)
                    (ignore-errors (eat-tmux-manager--current-record))))
         (seed-path (and seed (plist-get seed :path)))
         (default-directory (if (and (stringp seed-path)
                                     (not (string-empty-p seed-path)))
                                seed-path
                              default-directory)))
    (call-interactively #'eat-tmux-open)))

(defun eat-tmux-manager-kill-session ()
  "Kill tmux session at point after confirmation."
  (interactive)
  (let* ((record (eat-tmux-manager--current-record))
         (session (plist-get record :session))
         (remote (plist-get record :remote))
         (source (or (plist-get record :source) "tmux")))
    (unless (and (stringp session) (not (string-empty-p session)))
      (user-error "Missing tmux session in row"))
    (when (y-or-n-p (format "Kill tmux session %s on %s? " session source))
      (if remote
          (eat-tmux--ensure-ssh)
        (eat-tmux--ensure-tmux))
      (eat-tmux--tmux-command-require-success remote
                                              "kill-session" "-t" session)
      (eat-tmux-manager-refresh)
      (message "Killed tmux session: %s" session))))

(defvar eat-tmux-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'eat-tmux-manager-refresh)
    (define-key map (kbd "o") #'eat-tmux-manager-visit)
    (define-key map (kbd "v") #'eat-tmux-manager-visit)
    (define-key map (kbd "RET") #'eat-tmux-manager-visit)
    (define-key map (kbd "j") #'eat-tmux-manager-open-directory)
    (define-key map (kbd "D") #'eat-tmux-manager-kill-session)
    (define-key map (kbd "+") #'eat-tmux-manager-new)
    map)
  "Keymap for `eat-tmux-manager-mode'.")

(define-derived-mode eat-tmux-manager-mode tabulated-list-mode "Eat-Tmux-Manager"
  "Major mode for browsing and jumping between tmux sessions."
  (setq tabulated-list-format
        (vconcat
         (list
          '("Src" 18 t)
          '("Session" 30 t)
          (list "Path" eat-tmux-manager-path-column-width t)
          '("Windows" 0 t))))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Src" . nil))
  (tabulated-list-init-header)
  (eat-tmux-manager-refresh))

;;;###autoload
(defun eat-tmux-manager ()
  "Open the tmux session manager buffer."
  (interactive)
  (let ((buffer (get-buffer-create eat-tmux-manager--buffer-name)))
    (with-current-buffer buffer
      (eat-tmux-manager-mode))
    (pop-to-buffer buffer)))

(provide 'eat-tmux)
;;; eat-tmux.el ends here
