;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; gui related customizations

;;; Code:

(tool-bar-mode 0)
(menu-bar-mode 0)

(setq-default
 ns-use-proxy-icon nil
 frame-title-format nil
 frame-background-mode nil)

(setq default-frame-alist
      '((width . 84)
        (height . 44)
        (font . "Hack 14")
        (ns-transparent-titlebar . t)
        (ns-appearance . dark)))

(defun my/prepare-ui ()
  (setq ns-use-native-fullscreen nil)
  (menu-bar-mode 1)
  (scroll-bar-mode 0)
  (fringe-mode 8))

;; Runs for standalone GUI
(if (display-graphic-p)
    (progn
      (my/prepare-ui)
      (message "Standalone GUI settings done!")))

;; Runs for emacs client frame
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (my/prepare-ui)
              (message "Client GUI settings done!"))))

(defun my/buffer-face-mode-monospace ()
  "Set font to a constant width fonts in current buffer."
  (interactive)
  (setq buffer-face-mode-face
        '(:family "SF Mono" :height 140))
  (buffer-face-mode)
  (delight 'buffer-face-mode nil t))
(add-hook 'prog-mode-hook 'my/buffer-face-mode-monospace)

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-medium t))

(load "~/.emacs.d/my-modeline")

(use-package rainbow-mode
  :hook
  (emacs-lisp-mode
   sass-mode
   scss-mode
   web-mode
   css-mode)
  :delight)

(set-face-attribute 'fringe nil :background "#282828")

(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (set-face-background 'fringe "#3c3836")
  (run-with-idle-timer
   0.1 nil (lambda () (set-face-background 'fringe "#282828"))))
(setq visible-bell nil
      ring-bell-function #'my/terminal-visible-bell)

;;; configure_gui.el ends here
