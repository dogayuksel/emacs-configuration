;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Helm related settings

;;; Code:

(use-package helm
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-mini)
   ("C-c C-o" . helm-imenu)
   ("C-x c a" . helm-apropos)
   ("C-c m" . helm-all-mark-rings))
  :defines (helm-recentf-fuzzy-match
            helm-buffers-fuzzy-matching
            helm-M-x-fuzzy-match)
  :config
  (helm-mode 1)
  (setq helm-autoresize-max-height 30
        helm-autoresize-mode t
        helm-split-window-inside-p t
        helm-recentf-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t)
  :delight)

(use-package helm-descbinds
  :bind
  ("C-h b" . helm-descbinds))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'helm))

(use-package helm-ag
  :bind ("M-g s" . helm-do-ag-project-root)
  :commands (helm-do-ag
             helm-ag))

(defun *-popwin-help-mode-off ()
  "Turn `popwin-mode' off for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (delq 'help-mode popwin:special-display-config))))

(defun *-popwin-help-mode-on ()
  "Turn `popwin-mode' on for *Help* buffers."
  (when (boundp 'popwin:special-display-config)
    (customize-set-variable 'popwin:special-display-config
                            (add-to-list 'popwin:special-display-config
                                         'help-mode nil #'eq))))

(add-hook 'helm-minibuffer-set-up-hook #'*-popwin-help-mode-off)
(add-hook 'helm-cleanup-hook #'*-popwin-help-mode-on)

(use-package helm-bibtex
  :commands (helm-bibtex)
  :config
  (setq bibtex-completion-bibliography
        '("~/Dropbox/.org/bibtex/file-1.bib"
          "~/Dropbox/.org/bibtex/file-2.bib")
        bibtex-entry-format
        '(opts-or-alts required-fields numerical-fields realign)
        bibtex-completion-library-path
        '("~/Dropbox/Ebooks/bibtex-library")
        bibtex-completion-notes-path
        "~/Dropbox/.org/bibtex/bibtex-notes.org"
        bibtex-completion-pdf-symbol "⌘"
        bibtex-completion-notes-symbol "✎"
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil "-a" "/Applications/Skim.app" fpath))
        bibtex-completion-format-citation-functions
        '((org-mode . bibtex-completion-format-citation-cite)
          (latex-mode . bibtex-completion-format-citation-cite))))

;;; configure_helm.el ends here
