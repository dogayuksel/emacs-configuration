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
      `((width . 84)
        (height . 44)
        (font . ,(format
                  "Hack %d"
                  (+ 12 my/fontsize-offset)))
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
        `(:family "SF Mono"
                  :height ,(+ 120 (* my/fontsize-offset 10))))
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

(use-package golden-ratio
  :after (helm)
  :init (golden-ratio-mode 1)
  :config
  (progn
    (defun add-to-excluded-buffer-names (name)
      (add-to-list 'golden-ratio-exclude-buffer-names name))
    (mapc 'add-to-excluded-buffer-names '(" *transient*"
                                          " *Agenda Commands*"))
    (defun add-to-extra-commands (name)
      (add-to-list 'golden-ratio-extra-commands name))
    (mapc 'add-to-extra-commands '(evil-window-left
                                   evil-window-right
                                   evil-window-up
                                   evil-window-down))
    (defun pl/helm-alive-p ()
      (and (boundp 'helm-alive-p)
           (symbol-value 'helm-alive-p)))
    (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)
    (setq golden-ratio-recenter t))
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
