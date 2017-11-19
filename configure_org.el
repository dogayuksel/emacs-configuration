;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Org related configurations

;;; Code:

(load-file "~/.emacs.d/constants.elc")

(use-package org
  :mode
  (("\\.org$\\'" . org-mode)
   ("\\.md$\\'" . org-mode))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-iswitchb))
  :defines (org-capture-templates
            org-mobile-directory
            org-mobile-encryption-password
            my/org-mobile-encryption-password
            org-mobile-inbox-for-pull
            org-mobile-use-encryption
            buffer-face-mode-face)
  :config
  (progn
    (setq org-agenda-files
          (quote ("~/Dropbox/.org/.doga.org"
                  "~/Dropbox/.org/.working.org"
                  "~/Dropbox/.org/journal.org"
                  "~/Dropbox/.org/gcal.org"))
          org-archive-location "~/Dropbox/.org/archive/%s_archive::"
          org-refile-targets '((org-agenda-files :maxlevel . 2))
          org-outline-path-complete-in-steps nil
          org-refile-use-outline-path t
          org-catch-invisible-edits (quote show-and-error)
          org-directory "~/Dropbox/.org"
          org-export-initial-scope (quote subtree)
          org-capture-templates
          '(("t" "Todo" entry
             (file+headline "~/Dropbox/.org/.doga.org" "Tasks")
             "* TODO %?\n  %i\n  %a")
            ("a" "Appointment" entry
             (file  "~/Dropbox/.org/gcal.org" )
             "* %?\n %^T\n")
            ("j" "Journal" entry
             (file+datetree "~/Dropbox/.org/journal.org")
             "* %?\nEntered on %U\n  %i\n  %a"))
          org-mobile-directory "~/Dropbox/Apps/MobileOrg"
          org-mobile-encryption-password my/org-mobile-encryption-password
          org-mobile-inbox-for-pull "~/Dropbox/.org/from-mobile.org"
          org-mobile-use-encryption t)
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'latex-mode-hook 'visual-line-mode)
    (use-package visual-fill-column
      :init
      (setq-default
       visual-fill-column-center-text t
       visual-fill-column-width 90)
      :config
      (add-hook 'org-mode-hook 'visual-fill-column-mode)
      (add-hook 'org-agenda-mode-hook 'visual-fill-column-mode)
      (add-hook 'latex-mode-hook 'visual-fill-column-mode))
    (use-package org-bullets
      :init
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
      )
    (defun my/buffer-face-mode-variable ()
      "Set font to a variable width fonts in current buffer."
      (interactive)
      (setq buffer-face-mode-face
            '(:family "Avenir Book" :height 150 :weight light))
      (setq line-spacing '0.15)
      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
      (buffer-face-mode))
    (add-hook 'org-mode-hook 'my/buffer-face-mode-variable)
    (use-package ox-latex
      :ensure org
      :config
      (progn
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
              '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f  %f"))
        )
      )
    (use-package ox-md
      :ensure org
      )))

(use-package org-gcal
  :defines (org-gcal-client-id
            org-gcal-client-secret
            my/org-gcal-client-secret
            org-gcal-file-alist)
  :init
  (setq
   org-gcal-client-id
   "172076857986-m651ifu3fbc51usuet841lvp3m8t11g7.apps.googleusercontent.com"
   org-gcal-client-secret my/org-gcal-client-secret
   org-gcal-file-alist
   '(("dogayuksel@gmail.com" . "~/Dropbox/.org/gcal.org"))))
(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync)))

(setq reftex-default-bibliography
      (quote
       ("~/Dropbox/.org/bibtex/file-1.bib"
        "~/Dropbox/.org/bibtex/file-2.bib")))

;;; configure_org.el ends here
