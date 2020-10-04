;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Nothing much to say here..

;;; Code:

;; User Info
(if (file-exists-p "~/.emacs.d/.emacs_secrets.el")
    (progn
      (load "~/.emacs.d/.emacs_secrets")
      (setq user-full-name my/full-name
            user-email my/email)))

;; Sets garbage collection threshold to 500mb
(setq gc-cons-threshold (* 500 1024 1024))

;; bootstrap straight.el
(setq straight-repository-branch "develop")
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

;; tell straight.el to fetch packages declared by use-package
(setq straight-use-package-by-default t)

(use-package use-package-ensure-system-package)

(use-package delight)

(use-package auto-compile
  :init
  (setq load-prefer-newer t)
  :config
  (progn
    (auto-compile-on-load-mode)
    (auto-compile-on-save-mode)))

(defun my/add-to-multiple-hooks (function hooks)
  "Apply given FUNCTION to a list of HOOKS."
  (mapc (lambda (hook) (add-hook hook function)) hooks))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8a1310c6fd7148a4557c5e66a83cefa54e88d7abc392e145858a19307dfc72f4" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.emacs.d/evil")
(load "~/.emacs.d/basics")
(load "~/.emacs.d/configure_gui")
(load "~/.emacs.d/configure_org")
(load "~/.emacs.d/configure_prog")

(if (file-exists-p "~/.emacs.d/.vterm/emacs-libvterm")
    (load "~/.emacs.d/configure_libvterm"))

;; Sets garbage collection threshold back to 5mb
(setq gc-cons-threshold (* 100 1024 1024))

;; (benchmark-init/deactivate)

;;; init.el ends here
