;;; package --- various functions

;;; Commentary:

;;; Code:

(defun my/split-and-open-shell ()
  "Split window and start shell."
  (interactive)
  (split-window-right)
  (other-window 1)
  (shell))

(defun my/open-iterm-here ()
  "Read default directory and open iterm there."
  (interactive)
  (dired-smart-shell-command "open -a iTerm -n $PWD" nil nil))

(defun last-char-p (str char)
  "Check if given CHAR is the last in STR."
  (let ((length (length str)))
    (and (> length 0)
         (char-equal (elt str (- length 1)) char))))

(defun chop-newline (str)
  "Remove new line at the end of STR if it exists."
  (let ((length (length str)))
    (if (last-char-p str ?\n)
        (substring str 0 (- length 1))
      str)))

(defun my/send-line-to-iterm ()
  "Send current line to the running iTerm instance."
  (interactive)
  (let* ((command (thing-at-point 'line))
         (str (concat "osascript "
                      "-e 'tell app \"iTerm\"' "
                      "-e 'tell current window' "
                      "-e 'tell current session' "
                      "-e 'delay 0.05' "
                      "-e 'write text \""
                      (chop-newline command)
                      "\"' "
                      "-e 'end tell' "
                      "-e 'end tell' "
                      "-e 'end tell' ")))
    (shell-command str)))

(defun my/append-semicolon-to-the-end-of-line ()
  "Add a semicolon to the end of the line."
  (interactive)
  (end-of-line)
  (insert ";"))

(defun my/nuke-all-buffers ()
  "Confirm and kill all buffers."
  (interactive)
  (if (y-or-n-p "Do you want to kill all the buffers? ")
      (progn
        (mapc 'kill-buffer (buffer-list))
        (delete-other-windows)
        (message "Killed all the buffers.."))
    (progn
      (message "Not killing all"))))

(defun my/insert-full-name ()
  "Insert full name."
  (interactive)
  (insert user-full-name))

(defun my/set-frame-alpha (arg &optional active)
  "Used to change the transparency.  ARG (1-100).  ACTIVE."
  (interactive "nEnter alpha value (1-100): \np")
  (let* ((elt (assoc 'alpha default-frame-alist))
         (old (frame-parameter nil 'alpha))
         (new (cond ((atom old)     `(,arg ,arg))
                    ((eql 1 active) `(,arg ,(cadr old)))
                    (t              `(,(car old) ,arg)))))
    (if elt (setcdr elt new)
      (push `(alpha ,@new) default-frame-alist))
    (set-frame-parameter nil 'alpha new)))

(defvar my/init-files
  '("./init.el"
    "./.emacs_secrets.el"
    "./basics.el"
    "./configure_gui.el"
    "./configure_helm.el"
    "./configure_libvterm.el"
    "./configure_org.el"
    "./configure_prog.el"
    "./constants.el"
    "./evil.el"
    "./my-modeline.el"
    "./lib/personal/personal.el"
    "./lib/personal/ripgrep-transient.el"
    "./lib/personal/commercetools.el"))

(defun my/byte-compile-init-files ()
  "Compile all init files."
  (interactive)
  (mapc
   (lambda (file-path)
     (let ((expanded-path (expand-file-name file-path)))
       (if (file-exists-p expanded-path)
           (byte-compile-file (expand-file-name expanded-path))
         (message (format "file missing: %s" expanded-path)))))
   my/init-files))

(provide 'personal)
;;; personal.el ends here
