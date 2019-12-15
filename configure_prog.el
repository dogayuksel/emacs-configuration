;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; programming related configurations

;;; Code:

(use-package impatient-mode :commands (impatient-mode))

(use-package yaml-mode :mode "\\.yml\\'")

(use-package json-mode
  :mode "\\.json$\\'"
  :config
  (defun my/adjust-js-indentation ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))
  (add-hook 'json-mode-hook 'my/adjust-js-indentation))

(use-package graphql-mode :mode "\\.graphql\\'")

(use-package sass-mode :mode "\\.sass\\'")

(use-package scss-mode :mode "\\.scss\\'")

(use-package add-node-modules-path)

(use-package css-mode
  :config
  (progn
    (setq css-indent-offset 2)
    (add-hook 'css-mode-hook #'add-node-modules-path)))

(use-package web-mode
  :after (flycheck company)
  :mode ("\\.html\\'")
  :defines web-mode-content-types-alist
  :config
  (progn
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (add-hook 'web-mode-hook #'add-node-modules-path)
    (add-hook
     'web-mode-hook
     (lambda ()
       (when (string-equal "tsx" (file-name-extension buffer-file-name))
         (progn
           (setup-tide-mode)
           (flycheck-add-mode 'typescript-tslint 'web-mode)))))
    ;; enable typescript-tslint checker
    (setq web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-enable-current-element-highlight t
          web-mode-enable-current-column-highlight t
          web-mode-enable-auto-quoting nil)
    (set-face-attribute
     'web-mode-current-element-highlight-face
     nil :foreground "#aeee00")
  ;;; For better jsx syntax-highlighting in web-mode.
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      "Some decoration for jsx mode."
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))))

(use-package flycheck-flow
  :after (flycheck)
  :config
  (progn
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
    (add-hook 'web-mode-hook #'add-node-modules-path)))

;;; Company-flow is added to backends below grouped with tern.
(use-package company-flow
  :after (company)
  :config
  (add-hook 'web-mode-hook #'add-node-modules-path))

(use-package js2-mode
  :after (flycheck)
  :config
  (progn
    (setq js-indent-level 2)
    (flycheck-add-mode 'javascript-eslint 'js2-mode)
    (add-hook 'web-mode-hook #'add-node-modules-path)))

(use-package flow-js2-mode :delight)

(use-package js2-refactor
  :delight
  :after (js2-mode)
  :config
  (progn
    (setq js2-skip-preprocessor-directives t)
    (add-hook 'js2-mode-hook #'js2-refactor-mode)))

(use-package prettier-js :delight)

(use-package rjsx-mode
  :mode ("\\.js\\'")
  :config
  (progn
    (add-hook 'rjsx-mode-hook #'add-node-modules-path)
    (add-hook 'rjsx-mode-hook #'prettier-js-mode)))

(use-package indium
  :ensure-system-package
  (indium . "npm i -g indium"))

(defun my/add-to-multiple-hooks (function hooks)
  "Apply given FUNCTION to a list of HOOKS."
  (mapc (lambda (hook) (add-hook hook function)) hooks))

;;; Enable tern for javascript suggestions.
(use-package tern
  :delight
  :config
  (progn
    (setq tern-command '("~/.emacs.d/straight/repos/tern/bin/tern"))
    (my/add-to-multiple-hooks
     '(lambda ()
        (progn
          (if (not (string-equal " *helm dumb jump persistent*" (buffer-name)))
              (tern-mode t))))
     '(web-mode-hook js2-mode-hook rjsx-mode-hook))))

(use-package company-tern
  :after (company)
  :config
  (add-to-list 'company-backends '(company-tern company-flow)))

(defun setup-tide-mode ()
  "Setup tide mode."
  (tide-setup)
  (tide-hl-identifier-mode +1)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq tide-format-options '(:indentSize 2 :tabSize 2)))

(use-package tide :init (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  (setq coffee-tab-width 2)
  ;; generating sourcemap by '-m' option.
  ;; And you must set '--no-header' option
  (setq coffee-args-compile '("-c" "--no-header" "-m"))
  (use-package sourcemap)
  (add-hook 'coffee-after-compile-hook
            'sourcemap-goto-corresponding-point)
  ;; If you want to remove sourcemap file
  ;; after jumping corresponding point
  (defun my/coffee-after-compile-hook (props)
    (sourcemap-goto-corresponding-point props)
    (delete-file (plist-get props :sourcemap)))
  (add-hook 'coffee-after-compile-hook
            'my/coffee-after-compile-hook))

(use-package js-doc
  :straight
  (:type git :host github :repo "mooz/js-doc" :branch "master"
         :fork (:host github :repo "dogayuksel/js-doc")))

(use-package elpy
  :defer 10
  :config
  (setq elpy-rpc-backend "jedi")
  (setq elpy-rpc-python-command "python")
  (elpy-enable))

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

(use-package flycheck-rust)

(use-package rust-mode
  :after (flycheck-rust)
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package cargo :after (rust-mode))

(use-package toml-mode :mode ("\\.toml\\'" . toml-mode))

(use-package solidity-mode)

(defun shell-cmd (cmd)
  "Return the stdout output of a CMD if the command returned an error."
  (car (ignore-errors (apply 'process-lines (split-string cmd)))))

;; https://github.com/reasonml/reason-cli
(use-package reason-mode
  :after (merlin)
  :init
  (let* ((refmt-bin (shell-cmd "which refmt")))
    ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when refmt-bin
      (setq refmt-command refmt-bin)))
  :config
  (add-hook
   'reason-mode-hook
   (lambda () (add-hook 'before-save-hook 'refmt-before-save)
     (merlin-mode))))

(use-package merlin
  :init
  (let* ((merlin-bin (shell-cmd "which ocamlmerlin"))
         (merlin-base-dir
          (when merlin-bin
            (replace-regexp-in-string "bin/ocamlmerlin$" "" merlin-bin))))
    ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when merlin-bin
      (add-to-list 'load-path (concat merlin-base-dir "share/emacs/site-lisp/"))
      (setq merlin-command merlin-bin)))
  :config
  (add-to-list 'company-backends 'merlin-company-backend))

;;; configure_progn.el ends here
