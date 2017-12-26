;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; elpy and python configurations

;;; Code:

(use-package elpy
  :defer 10
  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python")
  (elpy-enable))

;; (defun my-fix-tabs ()
;;   "Fix tab spaces for python."
;;   (setq tab-width 4)
;; )

;; (add-hook 'elpy-mode-hook #'my-fix-tabs)

;; (require 'mmm-mode)
;; (setq mmm-global-mode 'maybe)
;; (mmm-add-classes
;;  '((python-rst
;;     :submode rst-mode
;;     :front "^ *[ru]?\"\"\"[^\"]*$"
;;     :back "^ *\"\"\""
;;     :include-front t
;;     :include-back t
;;     :end-not-begin t)))
;; (mmm-add-mode-ext-class 'python-mode nil 'python-rst)
;; (setq font-lock-global-modes '(not in rst-mode ...))

;; ;;; Auto parse python&docstring after save
;; (defun my-parse-before-save-hook ()
;;   "Parse for mmm mode on save."
;;   (if (bound-and-true-p mmm-mode)
;;       (funcall 'mmm-parse-buffer)
;;       (message "mmm-mode is off"))
;; )
;; (add-hook 'before-save-hook #'my-parse-before-save-hook)

;; (setq-default mmm-submode-decoration-level 0)

;;; configure_elpy.el ends here
