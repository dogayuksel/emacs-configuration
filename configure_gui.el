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

(use-package emacs
  :delight
  (auto-revert-mode)
  (visual-line-mode)
  (buffer-face-mode))

(use-package badwolf-theme
  :config
  (load-theme 'badwolf t))

(load-file "~/.emacs.d/my-modeline.elc")

(set-face-attribute 'fringe nil :background "grey8")

(use-package rainbow-mode
  :delight
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
    (add-hook 'sass-mode-hook 'rainbow-mode)
    (add-hook 'web-mode 'rainbow-mode)))

(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (set-face-background 'fringe "#aeee00")
  (run-with-idle-timer 0.1 nil
                       (lambda ()
                         (set-face-background 'fringe "grey8"))))

(setq visible-bell nil
      ring-bell-function #'my/terminal-visible-bell)

;;; configure_gui.el ends here
