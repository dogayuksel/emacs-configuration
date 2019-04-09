;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Nothing much to say here..

;;; Code:

;; User Info
(setq user-full-name "Doğa Yüksel")
(setq user-mail-address "dogayuksel@gmail.com")

;; Sets garbage collection threshold to 500mb
(setq gc-cons-threshold (* 500 1024 1024))

;; bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (require 'benchmark-init)
;; (benchmark-init/activate)

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(require 'use-package)
;; (eval-when-compile
;;   (require 'use-package))
;; (require 'bind-key)
;; (require 'diminish)

(use-package delight)

(use-package auto-compile
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

(use-package try :defer t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8d5f22f7dfd3b2e4fc2f2da46ee71065a9474d0ac726b98f647bc3c7e39f2819" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(if (file-exists-p "~/.emacs.d/.emacs_secrets.el")
    (load "~/.emacs.d/.emacs_secrets"))

(load "~/.emacs.d/basics")
(load "~/.emacs.d/configure_gui")
(load "~/.emacs.d/configure_helm")
(load "~/.emacs.d/configure_org")
(load "~/.emacs.d/configure_web")
(load "~/.emacs.d/configure_nonweb")

;; Sets garbage collection threshold back to 5mb
(setq gc-cons-threshold (* 5 1024 1024))

;; (benchmark-init/deactivate)

;;; init.el ends here
