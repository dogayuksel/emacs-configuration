;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Basic small packages

;;; Code:

;; customize basic behavior
(setq-default comint-completion-autolist t
              comint-move-point-for-output t
              comint-scroll-to-bottom-on-input t
              indent-tabs-mode nil
              tab-width 2
              visible-mark-max 1
              frame-background-mode nil
              inhibit-startup-screen t
              initial-scratch-message nil
              calendar-week-start-day 1
              mac-command-modifier nil
              mac-option-modifier (quote meta)
              mac-right-option-modifier nil)

(bind-key* "C-x p" 'previous-buffer)
(bind-key* "C-x n" 'next-buffer)
(bind-key* "C-c 1" 'comment-region)
(bind-key* "C-c 2" 'uncomment-region)

(windmove-default-keybindings)

;; Enable recentf mode, save files periodically
;; Increase the size of stored items
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)
(setq-default recentf-max-saved-items 50)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package synonyms
  :commands (synonyms)
  :init
  (setq synonyms-file "~/.emacs.d/synonyms/mthesaur.txt")
  (setq synonyms-cache-file  "~/.emacs.d/synonyms/mthesaur_cache.txt"))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package org-pomodoro
  :commands (org-pomodoro)
  :config
  (progn
    (use-package alert
      :defines (org-pomodoro-play-sounds
                alert-default-style)
      :config
      (setq
       alert-default-style (quote osx-notifier)
       org-pomodoro-play-sounds nil))))

(use-package auto-complete
  :disabled
  :diminish auto-complete-mode
  :config
  (ac-config-default))

(use-package company
  :defines (company-dabbrev-downcase)
  :config
  (progn
    (setq company-global-modes '(not org-mode)
          company-dabbrev-downcase nil)
    (global-company-mode)))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (progn
    ;;; flycheck global on
    (add-hook 'after-init-hook #'global-flycheck-mode)
    ;;; turn off jshint
    (setq flycheck-disabled-checkers
          (append flycheck-disabled-checkers
                  '(javascript-jshint)
                  '(python-pycompile)))
    ;;; webmode eslint
    (use-package flycheck-flow)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-mode 'javascript-flow 'web-mode)
    (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
    ;;; flycheck temp file
    (setq flycheck-temp-prefix ".flycheck")))

(use-package yasnippet
  :defer 7
  :config
  (progn
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/yasnippet-snippets")
    (yas-global-mode 1)
    (use-package helm-c-yasnippet
      :bind ("C-c y" . helm-yas-complete)
      :config
      (setq helm-yas-space-match-any-greedy t))))

(use-package avy
  :bind* ("C-c SPC" . avy-goto-char))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package iedit
  :bind ("C-c o" . iedit-mode))

(use-package drag-stuff
  :diminish drag-stuff-mode
  :bind
  (("<M-down>" . drag-stuff-down)
   ("<M-up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode 1))

(use-package neotree
  :bind ("C-c n" . neotree))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package dash-at-point
  :commands (dash-at-point
             dash-at-point-with-docset))

(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (add-hook 'web-mode-hook #'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
    (add-hook 'lisp-interaction-mode-hook #'smartparens-mode)))

(use-package multiple-cursors
  :bind
  (("C-." . mc/mark-next-like-this)
   ("C-," . mc/mark-previous-like-this)))

(use-package bookmark+
  :defines
  (bmkp-bmenu-state-file
   bookmark-default-file
   bmkp-last-as-first-bookmark-file)
  :init
  (setq bmkp-bmenu-state-file "/Users/doga/.emacs.d/.emacs-bmk-bmenu-state.el"
        bookmark-default-file "/Users/doga/.emacs.d/bookmarks"
        bmkp-last-as-first-bookmark-file nil))

(use-package dired+)

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (global-hungry-delete-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (progn
    (global-aggressive-indent-mode 1)
    (setq aggressive-indent-excluded-modes
          (append aggressive-indent-excluded-modes
                  '(coffee-mode
                    python-mode
                    jade-mode
                    sass-mode)))))

(use-package expand-region
  :bind ("C-c e" . er/expand-region))

;;; Quick access to the shell
(defun my/split-and-open-shell ()
  "Split window and start shell."
  (interactive)
  (split-window-right)
  (other-window 1)
  (shell))
(bind-key* "C-c p" 'my/split-and-open-shell)

(defun my/open-iterm-here ()
  "Read default directory and open iterm there."
  (interactive)
  (dired-smart-shell-command "open -a iTerm -n $PWD" nil nil))

;;; Buffer cleaner
(defun my/nuke-all-buffers ()
  "Confirm and kill all buffers."
  (interactive)
  (if (y-or-n-p "Do you want to kill all the buffers? ")
      (progn
        (mapc 'kill-buffer (buffer-list))
        (delete-other-windows)
        (message "Killed all the buffers.."))
    (progn
      (message "Not killing all"))))
(bind-key* "C-x K" 'my/nuke-all-buffers)

;;; basics.el ends here
