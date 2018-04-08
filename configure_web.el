;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; web related configurations

;;; Code:

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

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package impatient-mode
  :commands (impatient-mode))

(use-package web-mode
  :mode ("\\.html\\'"
         "\\.jsx?\\'"
         "\\.tsx\\'")
  :defines web-mode-content-types-alist
  :init
  (setq web-mode-content-types-alist
        '(("json" . "\\.json\\'")
          ("jsx" . "\\.jsx\\'")))
  :config
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

(defun setup-tide-mode ()
  "Setup tide mode."
  (tide-setup)
  (tide-hl-identifier-mode +1)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (setq tide-format-options '(:indentSize 2 :tabSize 2)))

(use-package tide
  :init
  (add-hook
   'typescript-mode-hook #'setup-tide-mode)
  (add-hook
   'web-mode-hook
   (lambda ()
     (when (string-equal
            "tsx" (file-name-extension buffer-file-name))
       (setup-tide-mode)))))

;;; Enable tern for javascript suggestions
(add-to-list 'load-path "~/.emacs.d/tern-project/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(use-package company-tern
  :after (company)
  :config
  (add-to-list 'company-backends 'company-tern))

(defun my/tern-setup ()
  "Start tern mode."
  (tern-mode t))
(add-hook 'web-mode-hook 'my/tern-setup)

;;; configure_web.el ends here
