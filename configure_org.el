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
  :defines (org-export-initial-scope
            org-capture-templates
            org-mobile-directory
            org-mobile-encryption-password
            my/org-mobile-encryption-password
            org-mobile-inbox-for-pull
            org-mobile-use-encryption
            buffer-face-mode-face
            org-agenda-custom-commands)
  :init
  (setq
   org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
  :config
  (progn
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
                "~/Dropbox/.org/.working.org"
                "~/Dropbox/.org/journal.org"
                "~/Dropbox/.org/gcal.org")
              org-archive-location "~/Dropbox/.org/archive/%s_archive::"
              org-directory "~/Dropbox/.org"
              org-capture-templates
              '(("t" "Todo" entry
                 (file+headline "~/Dropbox/.org/.doga.org" "Tasks")
                 "* TODO %?\n  %i\n  %a")
                ("a" "Appointment" entry
                 (file  "~/Dropbox/.org/gcal.org" )
                 "* %?\n %^T\n")
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
                ("gt" "Travelling" tags-todo "@travelling")
                ("G" "GTD Block Agenda"
                 ((tags-todo "@office")
                  (tags-todo "@hacking")
                  (tags-todo "@phone")
                  (tags-todo "@email")
                  (tags-todo "@home")
                  (tags-todo "@errands")
                  (tags-todo "@travelling"))
                 nil                      ;; i.e., no local settings
                 ("~/Dropbox/.org/next-actions.txt"))
                ;; exports block to this file with C-c a e
                ;; ..other commands here
                )))
    ;; pull contents when org-mode is loaded
    (if (boundp 'my/org-mobile-encryption-password)
        (progn
          (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
                org-mobile-encryption-password
                my/org-mobile-encryption-password
                org-mobile-inbox-for-pull
                "~/Dropbox/.org/from-mobile.org"
                org-mobile-use-encryption t)
          (org-mobile-pull)
          ;; push changes when emacs is killed
          ;; and org-mode is already loaded
          (add-hook 'kill-emacs-hook 'org-mobile-push)))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (js . t)
       (python . t)
       (ditaa . t)))
    (defun my/setup-visual-line-mode ()
      (progn
        (visual-line-mode)
        (delight 'visual-line-mode nil t)))
    (add-hook 'org-mode-hook 'my/setup-visual-line-mode)
    (add-hook 'latex-mode-hook 'my/setup-visual-line-mode)
    (defun my/buffer-face-mode-variable ()
      "Set font to a variable width fonts in current buffer."
      (interactive)
      (setq buffer-face-mode-face
            '(:family "Avenir Book" :height 130 :weight light))
      (setq line-spacing '0.15)
      (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
      (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
      (buffer-face-mode))
    (add-hook 'org-mode-hook 'my/buffer-face-mode-variable)
    (add-hook
     'org-mode-hook
     '(lambda ()
        (push '("-->" . ?➡) prettify-symbols-alist)))))

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

(use-package org-pomodoro
  :defines (org-pomodoro-play-sounds)
  :commands (org-pomodoro)
  :after (org)
  :config
  (setq org-pomodoro-play-sounds nil))

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
  :if (boundp 'my/org-gcal-client-secret)
  :defines (org-gcal-client-id
            org-gcal-client-secret
            my/org-gcal-client-id
            my/org-gcal-client-secret
            org-gcal-file-alist)
  :init
  (setq
   org-gcal-client-id my/org-gcal-client-id
   org-gcal-client-secret my/org-gcal-client-secret
   org-gcal-file-alist
   '(("dogayuksel@gmail.com" . "~/Dropbox/.org/gcal.org")))
  :after (org)
  :config
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync))))

;;; configure_org.el ends here
