;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Nothing much to say here..

;;; Code:

;; User Info
(setq user-full-name "Doğa Yüksel")
(setq user-mail-address "dogayuksel@gmail.com")

;; Sets garbage collection threshold to 500mb
(setq gc-cons-threshold (* 500 1024 1024))

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(setq load-prefer-newer t)
(package-initialize)

;; (require 'benchmark-init)
;; (benchmark-init/activate)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; (eval-when-compile
;;   (require 'use-package))
;; (require 'bind-key)
;; (require 'diminish)

(setq use-package-verbose nil)
(setq use-package-always-ensure t)

(use-package delight)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(use-package try
  :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("beeb5ac6b65fcccfe434071d4624ff0308b5968bf2f0c01b567d212bcaf66054" "946e871c780b159c4bb9f580537e5d2f7dba1411143194447604ecbaf01bd90c" default)))
 '(package-selected-packages
   (quote
    (powerline alert delight tide graphql-mode org-gcal dumb-jump company-tern company helm-c-yasnippet yasnippet helm-ag auto-compile try expand-region aggressive-indent hungry-delete avy which-key rainbow-mode dash-at-point sourcemap org-pomodoro helm synonyms yaml-mode benchmark-init bookmark+ dired+ sass-mode coffee-mode flycheck-flow web-mode visual-fill-column use-package undo-tree swiper smartparens scss-mode popwin org-bullets org neotree multiple-cursors mmm-mode magit json-mode impatient-mode iedit helm-descbinds helm-bibtex flycheck exec-path-from-shell elpy drag-stuff badwolf-theme auto-complete airline-themes))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-file "~/.emacs.d/.emacs_secrets.elc")
(load-file "~/.emacs.d/basics.elc")
(load-file "~/.emacs.d/configure_gui.elc")
(load-file "~/.emacs.d/configure_org.elc")
(load-file "~/.emacs.d/configure_web.elc")
(load-file "~/.emacs.d/configure_helm.elc")
(load-file "~/.emacs.d/configure_elpy.elc")

;; Sets garbage collection threshold back to 5mb
(setq gc-cons-threshold (* 5 1024 1024))

;; (benchmark-init/deactivate)

;;; init.el ends here
