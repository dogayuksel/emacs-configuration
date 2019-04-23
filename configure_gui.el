;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; gui related customizations

;;; Code:

(tool-bar-mode 0)
(menu-bar-mode 0)

(global-prettify-symbols-mode t)

(setq-default
 ns-use-proxy-icon nil
 frame-title-format nil
 frame-background-mode nil)

(setq default-frame-alist
      '((width . 84)
        (height . 44)
        (font . "Hack 12")
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
        '(:family "SF Mono" :height 120))
  (buffer-face-mode)
  (delight 'buffer-face-mode nil t))
(add-hook 'prog-mode-hook 'my/buffer-face-mode-monospace)

(use-package nord-theme
  :straight
  (:type git :host github :repo "arcticicestudio/nord-emacs" :branch "develop")
  :config
  (progn
    (setq nord-comment-brightness 10)
    (load-theme 'nord t)))

(load "~/.emacs.d/my-modeline")

(use-package rainbow-mode
  :hook
  (emacs-lisp-mode
   sass-mode
   scss-mode
   web-mode
   css-mode)
  :delight)

(set-face-attribute 'fringe nil :background "#434C5E")

(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (set-face-background 'fringe "#81A1C1")
  (run-with-idle-timer
   0.1 nil (lambda () (set-face-background 'fringe "#434C5E"))))
(setq visible-bell nil
      ring-bell-function #'my/terminal-visible-bell)

;;; configure_gui.el ends here
