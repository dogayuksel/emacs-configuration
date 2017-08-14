;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; web related configurations

;;; Code:

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  (progn
    (setq coffee-tab-width 2)
    ;; generating sourcemap by '-m' option.
    ;; And you must set '--no-header' option
    (setq coffee-args-compile '("-c" "--no-header" "-m"))
    (use-package sourcemap)
    (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)
    ;; If you want to remove sourcemap file
    ;; after jumping corresponding point
    (defun my/coffee-after-compile-hook (props)
      (sourcemap-goto-corresponding-point props)
      (delete-file (plist-get props :sourcemap)))
    (add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)))

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package json-mode
  :mode "\\.json$\\'"
  :config
  (defun my/adjust-js-indentation ()
    (make-local-variable 'js-indent-level)
    (setq js-indent-level 2))
  (add-hook 'json-mode-hook 'my/adjust-js-indentation))

(use-package graphql-mode
  :mode "\\.graphql\\'")

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package impatient-mode
  :commands (impatient-mode))

(use-package web-mode
  :mode ("\\.html\\'"
         "\\.css\\'"
         "\\.jsx?\\'")
  :defines web-mode-content-types-alist
  :init
  (setq web-mode-content-types-alist
        '(("json" . "\\.json\\'")
          ("jsx"  . "\\.jsx\\'")))
  :config
  (progn
    (setq web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-script-padding 1
          web-mode-style-padding 1
          web-mode-block-padding 0
          web-mode-attr-indent-offset 2
          web-mode-enable-current-element-highlight t
          web-mode-enable-current-column-highlight t)
    (set-face-attribute
     'web-mode-current-element-highlight-face nil :background "gray9")
    (set-face-attribute
     'web-mode-current-column-highlight-face nil :background "gray15")
    ;;; For better jsx syntax-highlighting in web-mode.
    (defadvice web-mode-highlight-part (around tweak-jsx activate)
      "Some decoration for jsx mode."
      (if (equal web-mode-content-type "jsx")
          (let ((web-mode-enable-part-face nil))
            ad-do-it)
        ad-do-it))))

;;; Enable tern for javascript suggestions
(add-to-list 'load-path "~/.emacs.d/tern-project/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern))

(defun my/tern-setup ()
  "Start tern mode."
  (tern-mode t))
(add-hook 'web-mode-hook 'my/tern-setup)

;;; configure_web.el ends here
