;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; programming related configurations

;;; Code:

(use-package emr)

(use-package compile
  :config
  (progn
    ;; Parse error messages emitted by node.js
    ;; https://emacs.stackexchange.com/questions/27213/how-can-i-add-a-compilation-error-regex-for-node-js
    (push 'node compilation-error-regexp-alist)
    (push
     '(node
       "at [^ ]+ (\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3)
     compilation-error-regexp-alist-alist)))

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

(use-package lsp-mode
  :init
  (progn
    (setq read-process-output-max (* 1024 1024))
    (setq lsp-keymap-prefix "M-s M-l")
    (setq lsp-disabled-clients '((json-mode . eslint))))
  :hook
  ((js-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration))
  :config
  (progn
    (setq lsp-enable-indentation nil
          lsp-eldoc-hook nil))
  :commands lsp)

(use-package lsp-ui
  :config
  (progn
    (setq lsp-ui-doc-include-signature t
          lsp-ui-doc-delay 2))
  :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(use-package prettier-js :delight)

(use-package js
  :after (flycheck)
  :mode
  ("\\.[j|t]sx?\\'" . js-mode)
  :config
  (setq js-indent-level 2)
  :hook
  ((js-mode . add-node-modules-path)
   (js-mode . prettier-js-mode)))

(use-package indium
  :ensure-system-package
  (indium . "npm i -g indium"))

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

(use-package glsl-mode)

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
  :after (merlin projectile)
  :init
  (let* ((refmt-bin (shell-cmd "which bsrefmt")))
    ;; Add merlin.el to the emacs load path and tell emacs where to find ocamlmerlin
    (when refmt-bin
      (setq refmt-command refmt-bin)))
  :config
  (projectile-register-project-type
   'reasonml '("bsconfig.json")
   :compile "yarn start"
   :test "yarn test"
   :run "yarn server"
   :test-suffix "_test")
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
