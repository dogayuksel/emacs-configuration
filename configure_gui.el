;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; gui related customizations

;;; Code:

(menu-bar-mode 0)

(defvar my/fontsize-offset 0)
(defvar my/scaled-font-height 13)
(defvar my/monospace-font "Menlo")
(defvar my/variable-width-font "Helvetica")

(setq
 my/scaled-font-height (+ 13 my/fontsize-offset)
 my/monospace-font "JetBrains Mono"
 my/variable-width-font "Inter")

;; Ligature support from emacs-mac port
;; (mac-auto-operator-composition-mode)

;; Replace lambda with λ
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)

(setq default-frame-alist
      `((width . 84)
        (height . 44)
        (undecorated . t)
        (font . ,(format
                  "%s %d"
                  my/monospace-font
                  my/scaled-font-height))))

(defun my/prepare-gui ()
  "Configure GUI."
  (setq-default
   ns-use-native-fullscreen nil
   frame-title-format nil
   line-spacing 0.2)
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (fringe-mode 8))

;; Runs for standalone GUI
(if (display-graphic-p)
    (progn
      (my/prepare-gui)
      (message "Standalone GUI settings done!")))

;; Runs for emacs client frame / consecutive frames
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (when (display-graphic-p frame)
     (my/prepare-gui)
     (message "Client / New Frame GUI settings done!"))))

(use-package face-remap
  :delight buffer-face-mode
  :config
  (progn
    (defun my/buffer-face-mode-monospace ()
      "Set font to a constant width fonts in current buffer."
      (interactive)
      (setq
       buffer-face-mode-face
       `(:family ,my/monospace-font :height ,(* 10 my/scaled-font-height)))
      (buffer-face-mode))
    (add-hook 'prog-mode-hook 'my/buffer-face-mode-monospace)
    (defun my/buffer-face-mode-variable ()
      "Set font to a variable width fonts in current buffer."
      (interactive)
      (setq
       buffer-face-mode-face
       `(:family ,my/variable-width-font :height ,(* 10 my/scaled-font-height)))
      (set-face-attribute 'fixed-pitch nil :family my/monospace-font)
      (mapc
       (lambda (face) (set-face-attribute face nil :inherit 'fixed-pitch))
       (list 'org-code
             'org-link
             'org-block
             'org-table
             'org-verbatim
             'org-block-begin-line
             'org-block-end-line
             'org-meta-line
             'org-document-info-keyword))
      (buffer-face-mode))
    (add-hook 'org-mode-hook 'my/buffer-face-mode-variable)))

(use-package nord-theme
  :straight
  (:type git :host github :repo "arcticicestudio/nord-emacs" :branch "develop"
         :fork (:host github :repo "dogayuksel/nord-emacs"))
  :config (load-theme 'nord t))

(use-package telephone-line
  :config
  (progn
    (telephone-line-defsegment* my-telephone-line-buffer-modified-segment ()
      (if (buffer-modified-p)
          (telephone-line-raw "*")
        (telephone-line-raw "-")))
    (telephone-line-defsegment* my-telephone-line-vc-segment ()
      (telephone-line-raw
       (if (> (length vc-mode) 15)
           (format "%s…" (substring vc-mode 5 13))
         vc-mode)
       t))
    (setq telephone-line-lhs
          '((evil   . (telephone-line-evil-tag-segment))
            (accent . (my-telephone-line-buffer-modified-segment
                       my-telephone-line-vc-segment))
            (nil    . (telephone-line-projectile-buffer-segment))))
    (setq telephone-line-rhs
          '((nil    . (telephone-line-misc-info-segment
                       telephone-line-process-segment))
            (accent . (telephone-line-major-mode-segment))
            (evil   . (telephone-line-position-segment))))
    (setq telephone-line-evil-use-short-tag t
          telephone-line-height 22)
    (telephone-line-mode 1)))

(use-package rainbow-mode
  :hook
  (emacs-lisp-mode
   sass-mode
   scss-mode
   web-mode
   css-mode)
  :delight)

(set-face-attribute 'fringe nil :background "#3B4252")
(setq my/fringe-bell-on nil)

(defun my/terminal-visible-bell ()
  "A friendlier visual bell effect."
  (unless my/fringe-bell-on
    (progn
      (setq my/fringe-bell-on t)
      (setq my/current-fringe-color (face-attribute 'fringe :background))
      (set-face-background 'fringe "#434C5E")
      (run-with-idle-timer
       0.1 nil
       (lambda () (progn
               (set-face-background 'fringe my/current-fringe-color)
               (setq my/fringe-bell-on nil)))))))

(setq visible-bell nil
      ring-bell-function #'my/terminal-visible-bell)

;;; configure_gui.el ends here
