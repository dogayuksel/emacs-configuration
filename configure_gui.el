;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; gui related customizations

;;; Code:

(tool-bar-mode 0)
(menu-bar-mode 0)

(setq default-frame-alist
      '((width . 80)   ; character
        (height . 310)   ; lines
        (font . "Hack 14")))

;; if dired-directory then dired-dir else %b
(setq frame-title-format
      (setq icon-title-format
            '(buffer-file-name "%f"
                               (dired-directory
                                dired-directory "%b"))))

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

(use-package badwolf-theme
  :config
  (load-theme 'badwolf t))

(load-file "~/.emacs.d/my-modeline.elc")

(use-package rainbow-mode
  :hook
  (emacs-lisp-mode
   sass-mode
   web-mode)
  :delight)

(set-face-attribute 'fringe nil :background "grey8")

(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (set-face-background 'fringe "#aeee00")
  (run-with-idle-timer
   0.1 nil (lambda () (set-face-background 'fringe "grey8"))))
(setq visible-bell nil
      ring-bell-function #'my/terminal-visible-bell)

;;; configure_gui.el ends here
