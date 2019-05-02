;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; web related configurations

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

(defun my/get-executable-at-dir (bin-dir)
  "Check for executable at BIN-DIR relative to project root.
Project root is assumed to be the folder with node_modules folder."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (exec (and root (expand-file-name bin-dir root))))
    (if (and exec (file-executable-p exec)) exec)))

(use-package css-mode
  :config
  (setq css-indent-offset 2)
  (add-hook
   'css-mode-hook
   '(lambda ()
      (setq-local
       flycheck-css-stylelint-executable
       (my/get-executable-at-dir
        "node_modules/stylelint/bin/stylelint.js")))))

(use-package web-mode
  :after (flycheck company)
  :mode ("\\.html\\'"
         "\\.jsx\\'")
  :defines web-mode-content-types-alist
  :init
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.jsx\\'")))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq-local
       flycheck-javascript-eslint-executable
       (my/get-executable-at-dir
        "node_modules/eslint/bin/eslint.js"))))
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
      ad-do-it)))

(use-package flycheck-flow
  :after (flycheck)
  :config
  (flycheck-add-mode 'javascript-flow 'web-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq-local
       flycheck-javascript-flow-executable
       (my/get-executable-at-dir
        "node_modules/flow-bin/vendor/flow")))))

;;; Company-flow is added to backends below grouped with tern.
(use-package company-flow
  :after (company)
  :config
  (add-hook
   'web-mode-hook
   '(lambda ()
      (setq-local
       company-flow-executable
       (my/get-executable-at-dir
        "node_modules/flow-bin/vendor/flow")))))

(use-package js2-mode
  :mode ("\\.js\\'")
  :config
  (setq js-indent-level 2))

(use-package indium
  :ensure-system-package
  (indium . "npm i -g indium"))

(defun my/add-to-multiple-hooks (function hooks)
  "Apply given FUNCTION to a list of HOOKS."
  (mapc (lambda (hook) (add-hook hook function)) hooks))

;;; Enable tern for javascript suggestions.
(use-package tern
  :config
  (progn
    (setq tern-command '("~/.emacs.d/straight/repos/tern/bin/tern"))
    (my/add-to-multiple-hooks
     '(lambda () (tern-mode t))
     '(web-mode-hook js2-mode-hook))))

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

;;; configure_web.el ends here
