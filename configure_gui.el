;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; gui related customizations

;;; Code:

(tool-bar-mode 0)
(menu-bar-mode 0)

(setq frame-title-format nil)

(setq line-spacing 0.125)

;; Ligature support from emacs-mac port
(mac-auto-operator-composition-mode)

;; Replace lambda with λ (mode dependent)
(global-prettify-symbols-mode t)

(setq default-frame-alist
      `((width . 84)
        (height . 44)
        (font . ,(format
                  "JetBrains Mono %d"
                  (+ 13 my/fontsize-offset)))))

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
        `(:family "JetBrains Mono"
                  :height ,(+ 130 (* my/fontsize-offset 10))))
  (buffer-face-mode)
  (delight 'buffer-face-mode nil t))
(add-hook 'prog-mode-hook 'my/buffer-face-mode-monospace)

(use-package nord-theme
  :straight
  (:type git :host github :repo "arcticicestudio/nord-emacs" :branch "develop"
         :fork (:host github :repo "dogayuksel/nord-emacs"))
  :config (load-theme 'nord t))

(load "~/.emacs.d/my-modeline")

(use-package rainbow-mode
  :hook
  (emacs-lisp-mode
   sass-mode
   scss-mode
   web-mode
   css-mode)
  :delight)

(set-face-attribute 'fringe nil :background "#3B4252")

(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (set-face-background 'fringe "#434C5E")
  (run-with-idle-timer
   0.1 nil (lambda () (set-face-background 'fringe "#3B4252"))))
(setq visible-bell nil
      ring-bell-function #'my/terminal-visible-bell)

;;; configure_gui.el ends here
