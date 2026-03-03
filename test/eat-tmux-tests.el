;;; eat-tmux-tests.el --- Tests for eat-tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(add-to-list
 'load-path
 (expand-file-name ".."
                   (file-name-directory
                    (or load-file-name buffer-file-name))))

(require 'eat-tmux)

(defconst eat-tmux-tests--remote-with-user
  '(:method "ssh" :user "paul" :host "sandbox" :port "2222" :directory "~/work")
  "Remote plist with explicit user and port.")

(defconst eat-tmux-tests--remote-no-user
  '(:method "ssh" :host "sandbox" :directory "~/work")
  "Remote plist with host-only target.")

(ert-deftest eat-tmux-ssh-target-with-user ()
  (should (equal (eat-tmux--ssh-target eat-tmux-tests--remote-with-user)
                 "paul@sandbox")))

(ert-deftest eat-tmux-ssh-target-without-user ()
  (should (equal (eat-tmux--ssh-target eat-tmux-tests--remote-no-user)
                 "sandbox")))

(ert-deftest eat-tmux-ssh-probe-args-include-options-port-and-command ()
  (let ((eat-tmux-ssh-probe-options
         '("-o" "BatchMode=yes" "-o" "ConnectTimeout=5")))
    (should (equal (eat-tmux--ssh-probe-args eat-tmux-tests--remote-with-user
                                             "printf ok")
                   '("-o" "BatchMode=yes"
                     "-o" "ConnectTimeout=5"
                     "-p" "2222"
                     "paul@sandbox"
                     "printf ok")))))

(ert-deftest eat-tmux-ssh-probe-args-omit-port-when-missing ()
  (let ((eat-tmux-ssh-probe-options '("-o" "BatchMode=yes")))
    (should (equal (eat-tmux--ssh-probe-args eat-tmux-tests--remote-no-user
                                             "printf ok")
                   '("-o" "BatchMode=yes" "sandbox" "printf ok")))))

(ert-deftest eat-tmux-ssh-command-output-string-success ()
  (let ((eat-tmux-ssh-probe-options '("-o" "BatchMode=yes"))
        (captured-args nil))
    (cl-letf (((symbol-function 'process-file)
               (lambda (_program _infile _buffer _display &rest args)
                 (setq captured-args args)
                 (insert "hello\n")
                 0)))
      (should (equal (eat-tmux--ssh-command-output-string
                      eat-tmux-tests--remote-with-user
                      "printf hello")
                     "hello\n"))
      (should (equal captured-args
                     '("-o" "BatchMode=yes"
                       "-p" "2222"
                       "paul@sandbox"
                       "printf hello"))))))

(ert-deftest eat-tmux-ssh-command-output-string-failure-returns-nil ()
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _)
               (insert "nope\n")
               255)))
    (should-not (eat-tmux--ssh-command-output-string
                 eat-tmux-tests--remote-with-user
                 "printf nope"))))

(ert-deftest eat-tmux-ssh-command-output-string-failure-signals-when-required ()
  (cl-letf (((symbol-function 'process-file)
             (lambda (&rest _)
               (insert "failed\n")
               255)))
    (let ((err (should-error
                (eat-tmux--ssh-command-output-string
                 eat-tmux-tests--remote-with-user
                 "printf nope"
                 t)
                :type 'user-error)))
      (should (string-match-p "Remote command failed (255): failed"
                              (error-message-string err))))))

(ert-deftest eat-tmux-ssh-command-output-string-sets-askpass-require-never ()
  (let ((process-environment nil)
        (captured-env nil)
        (eat-tmux-ssh-probe-disable-askpass t))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _)
                 (setq captured-env process-environment)
                 0)))
      (eat-tmux--ssh-command-output-string
       eat-tmux-tests--remote-with-user
       "printf ok"))
    (should (member "SSH_ASKPASS_REQUIRE=never" captured-env))))

(ert-deftest eat-tmux-ssh-command-output-string-skips-askpass-override-when-disabled ()
  (let ((process-environment '("FOO=1"))
        (captured-env nil)
        (eat-tmux-ssh-probe-disable-askpass nil))
    (cl-letf (((symbol-function 'process-file)
               (lambda (&rest _)
                 (setq captured-env process-environment)
                 0)))
      (eat-tmux--ssh-command-output-string
       eat-tmux-tests--remote-with-user
       "printf ok"))
    (should-not (member "SSH_ASKPASS_REQUIRE=never" captured-env))
    (should (member "FOO=1" captured-env))))

(ert-deftest eat-tmux-remote-command-output-string-falls-back-to-tramp ()
  (let ((tramp-called nil))
    (cl-letf (((symbol-function 'eat-tmux--ssh-command-output-string)
               (lambda (&rest _) nil))
              ((symbol-function 'eat-tmux--tramp-command-output-string)
               (lambda (_remote _command require-success)
                 (setq tramp-called require-success)
                 "from-tramp")))
      (should (equal (eat-tmux--remote-command-output-string
                      eat-tmux-tests--remote-with-user
                      "printf ok"
                      t)
                     "from-tramp"))
      (should tramp-called))))

(ert-deftest eat-tmux-remote-command-output-string-prefers-ssh-success ()
  (let ((tramp-called nil))
    (cl-letf (((symbol-function 'eat-tmux--ssh-command-output-string)
               (lambda (&rest _) "from-ssh"))
              ((symbol-function 'eat-tmux--tramp-command-output-string)
               (lambda (&rest _)
                 (setq tramp-called t)
                 "from-tramp")))
      (should (equal (eat-tmux--remote-command-output-string
                      eat-tmux-tests--remote-with-user
                      "printf ok")
                     "from-ssh"))
      (should-not tramp-called))))

(ert-deftest eat-tmux-remote-command-output-lines-splits-lines ()
  (cl-letf (((symbol-function 'eat-tmux--remote-command-output-string)
             (lambda (&rest _) "one\ntwo\n")))
    (should (equal (eat-tmux--remote-command-output-lines
                    eat-tmux-tests--remote-with-user
                    "printf lines")
                   '("one" "two")))))

(provide 'eat-tmux-tests)

;;; eat-tmux-tests.el ends here
