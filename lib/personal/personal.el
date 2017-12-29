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

(defun my/insert-fullname ()
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

(provide 'personal)
;;; personal.el ends here
