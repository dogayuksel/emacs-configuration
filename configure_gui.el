;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; gui related customizations

;;; Code:

(tool-bar-mode 0)
(menu-bar-mode 0)

(setq default-frame-alist
      '(
        (width . 80)   ; character
        (height . 310)   ; lines
        (font . "Hack 14")))

;; Runs for standalone GUI
(if (display-graphic-p)
    (progn
      (setq ns-use-native-fullscreen nil)
      (menu-bar-mode 1)
      (scroll-bar-mode 0)
      (fringe-mode 4)
      (message "Standalone GUI settings done!")))

;; Runs for emacs client frame
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (setq ns-use-native-fullscreen nil)
              (menu-bar-mode 1)
              (scroll-bar-mode 0)
              (fringe-mode 4)
              (message "Client GUI settings done!"))))

(defun my/buffer-face-mode-monospace ()
  "Set font to a constant width fonts in current buffer."
  (interactive)
  (setq buffer-face-mode-face
        '(:family "SF Mono" :height 140))
  (buffer-face-mode))
(add-hook 'prog-mode-hook 'my/buffer-face-mode-monospace)

(use-package badwolf-theme
  :config
  (load-theme 'badwolf t))

(load-file "~/.emacs.d/my-modeline.elc")

(set-face-attribute 'fringe nil :background "grey8")

(use-package rainbow-mode
  :mode ("\\.css\\'"
         "\\.sass\\'"
         "\\.scss\\'"))

(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (set-face-background 'fringe "#aeee00")
  (run-with-idle-timer 0.1 nil
                       (lambda ()
                         (set-face-background 'fringe "grey8"))))

(setq visible-bell nil
      ring-bell-function #'my/terminal-visible-bell)

(defun my/set-frame-alpha (arg &optional active)
  "Used to change the transparency.  ARG (1-100).  ACTIVE."
  (interactive "nEnter alpha value (1-100): \np")
  (let* ((elt (assoc 'alpha default-frame-alist))
         (old (frame-parameter nil 'alpha))
         (new (cond ((atom old)     `(,arg ,arg))
                    ((eql 1 active) `(,arg ,(cadr old)))
                    (t              `(,(car old) ,arg)))))
    (if elt (setcdr elt new) (push `(alpha ,@new) default-frame-alist))
    (set-frame-parameter nil 'alpha new)))

(bind-key* "C-c t" 'my/set-frame-alpha)

;;; configure_gui.el ends here
