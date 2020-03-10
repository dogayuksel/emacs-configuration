;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Org related configurations

;;; Code:

(load "~/.emacs.d/constants")

(use-package org
  :after (alert)
  :mode ("\\.org$\\'" . org-mode)
  :general
  ("C-c l" 'org-store-link
   "C-c a" 'org-agenda
   "C-c c" 'org-capture)
  :config
  (progn
    (add-to-list 'org-modules 'org-habit t)
    (add-hook 'org-mode-hook 'org-indent-mode)
    (setq org-refile-targets '((org-agenda-files :maxlevel . 3))
          org-reverse-note-order t
          org-outline-path-complete-in-steps nil
          org-refile-use-outline-path t
          org-catch-invisible-edits 'show-and-error
          org-export-initial-scope 'subtree
          org-todo-keywords '((sequence
                               "TODO(t)"
                               "WAITING(w)"
                               "|"
                               "DONE(d)"
                               "CANCELLED(c)"))
          org-show-notification-handler '(lambda (msg) (alert msg))
          org-log-into-drawer t)
    (if (file-exists-p "~/Dropbox/.org/.doga.org")
        (setq org-agenda-files
              '("~/Dropbox/.org/.doga.org"
                "~/Dropbox/.org/inbox.org"
                "~/Dropbox/.org/journal.org"
                "~/Dropbox/.org/gcal.org")
              org-archive-location "~/Dropbox/.org/archive/%s_archive::"
              org-directory "~/Dropbox/.org"
              org-capture-templates
              '(("t" "Todo" entry
                 (file "~/Dropbox/.org/inbox.org")
                 "* TODO %?\n  %i\n  %a")
                ("j" "Journal" entry
                 (file+olp+datetree "~/Dropbox/.org/journal.org")
                 "* %?\nEntered on %U\n  %i\n  %a"))
              org-tag-alist '(("@office")
                              ("@hacking")
                              ("@phone")
                              ("@email")
                              ("@home")
                              ("@errands")
                              ("@travelling"))
              org-agenda-custom-commands
              '(("g" . "GTD contexts")
                ("go" "Office" tags-todo "@office")
                ("gc" "Computer" tags-todo "@hacking")
                ("gp" "Phone" tags-todo "@phone")
                ("gm" "E-mail" tags-todo "@email")
                ("gh" "Home" tags-todo "@home")
                ("ge" "Errands" tags-todo "@errands")
                ("gt" "Travelling" tags-todo "@travelling"))))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (js . t)
       (python . t)
       (ditaa . t)))))

(use-package emacs
  :delight visual-line-mode
  :hook ((org-mode latex-mode) . visual-line-mode))

(use-package evil-org
  :after (org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook
   'evil-org-mode-hook
   (lambda ()
     (evil-org-set-key-theme
      '(textobjects insert navigation additional todo return heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (general-define-key
   :definer 'minor-mode
   :states '(normal visual)
   :keymaps 'evil-org-mode
   "j" 'evil-next-visual-line
   "k" 'evil-previous-visual-line)
  :delight)

(use-package org-super-agenda
  :after (org)
  :config
  (progn
    (setq org-super-agenda-header-map (make-sparse-keymap))
    (org-super-agenda-mode)
    (add-to-list
     'org-agenda-custom-commands
     '("G" "All Todos"
       ((todo "" ((org-super-agenda-groups
                   '((:name "Office" :and (:tag "@office" :todo "TODO"))
                     (:name "Hacking" :and (:tag "@hacking" :todo "TODO"))
                     (:name "Phone" :and (:tag "@phone" :todo "TODO"))
                     (:name "E-mail" :and (:tag "@email" :todo "TODO"))
                     (:name "Home" :and (:tag "@home" :todo "TODO"))
                     (:name "Errands" :and (:tag "@errands" :todo "TODO"))
                     (:name "Travelling" :and (:tag "@travelling" :todo "TODO"))
                     (:discard (:habit t)))))))))))

(use-package org-brain
  :after (org)
  :if (file-exists-p "~/Dropbox/.org/brain/index.org")
  :init
  (progn
    (setq org-brain-path "~/Dropbox/.org/brain")
    ;; For Evil users
    (with-eval-after-load 'evil
      (evil-set-initial-state 'org-brain-visualize-mode 'emacs)))
  :config
  (progn
    (setq org-id-track-globally t)
    (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 20)))

(use-package visual-fill-column
  :init
  (setq visual-fill-column-center-text t
        visual-fill-column-width 90)
  :after (org)
  :config
  (add-hook 'org-mode-hook 'visual-fill-column-mode)
  (add-hook 'org-agenda-mode-hook 'visual-fill-column-mode)
  (add-hook 'latex-mode-hook 'visual-fill-column-mode))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :after (org))

(use-package ox-latex
  :straight nil
  :after (org)
  :config
  (add-to-list 'org-latex-classes
               '("koma-article"
                 "\\documentclass{scrartcl}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("koma-book"
                 "\\documentclass{scrbook}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("scrlttr"
                 "\\documentclass[11pt]{scrlttr2}\n
               \\usepackage[utf8]{inputenc}\n
               \\usepackage[T1]{fontenc}\n
               \\usepackage{xcolor}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-caption-above nil
        org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f  %f")))

(use-package ivy-bibtex
  :commands (ivy-bibtex)
  :after (ivy)
  :config
  (setq
   bibtex-completion-bibliography
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
   bibtex-completion-format-citation-functions
   '((org-mode . bibtex-completion-format-citation-cite)
     (latex-mode . bibtex-completion-format-citation-cite))))

(use-package ox-md :straight nil :after (org))

(use-package reftex
  :config
  (setq reftex-default-bibliography
        '("~/Dropbox/.org/bibtex/file-1.bib"
          "~/Dropbox/.org/bibtex/file-2.bib")))

(use-package ox-reveal
  :after (org)
  :config
  (setq org-reveal-root "./reveal.js"))

(use-package org-gcal
  :straight
  (:type git :host github :repo "kidd/org-gcal.el")
  :after (org)
  :if (boundp 'my/org-gcal-client-secret)
  :defines (org-gcal-client-id
            org-gcal-client-secret
            my/org-gcal-client-id
            my/org-gcal-client-secret
            org-gcal-fetch-file-alist)
  :init
  (setq
   org-gcal-client-id my/org-gcal-client-id
   org-gcal-client-secret my/org-gcal-client-secret
   org-gcal-fetch-file-alist
   `((,my/calendar-address . "~/Dropbox/.org/gcal.org"))))

;;; configure_org.el ends here
