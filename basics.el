;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Basic small packages

;;; Code:

;; customize basic behavior

(setq-default
 inhibit-startup-screen t
 initial-scratch-message nil
 calendar-week-start-day 1)

(setq-default
 comint-completion-autolist t
 comint-move-point-for-output t
 comint-scroll-to-bottom-on-input t)

(setq-default
 indent-tabs-mode nil
 tab-width 2)

(setq-default
 mac-command-modifier 'meta
 mac-option-modifier 'meta
 mac-right-option-modifier nil)

(windmove-default-keybindings)

(use-package hydra)
(use-package transient)

(use-package matcha
  :after (hydra transient)
  :straight
  (:type git :host github :repo "jojojames/matcha"
         :fork (:host github :repo "dogayuksel/matcha"))
  :config (matcha-setup))

(use-package general
  :config
  (general-override-mode)
  (general-define-key
   :states '(normal motion)
   :keymaps 'override
   "SPC" 'matcha-me-space)
  (general-define-key
   :states 'emacs
   :keymaps 'override
   "C-SPC" 'matcha-me-space))

(use-package undo-tree
  :straight
  (:type git :host nil :repo "http://www.dr-qubit.org/git/undo-tree.git")
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :delight)

(use-package evil
  :init
  (setq
   evil-want-integration t
   evil-want-keybinding nil)
  :config
  (progn
    (evil-mode 1)
    (setq evil-want-fine-undo t)))

(use-package evil-collection
  :after (evil)
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround :after (evil) :config (global-evil-surround-mode 1))

(use-package evil-mc
  :after (hydra)
  :init
  (setq
   evil-mc-one-cursor-show-mode-line-text nil
   evil-mc-mode-line-text-cursor-color nil)
  :config
  (progn
    (global-evil-mc-mode 1)
    (mapc
     #'(lambda (x)
         (progn
           (define-key evil-mc-key-map (kbd x) nil)
           (evil-define-key 'normal evil-mc-key-map (kbd x) nil)
           (evil-define-key 'visual evil-mc-key-map (kbd x) nil)))
     '("C-p" "C-n" "C-t" "M-p" "M-n"))))

(use-package browse-kill-ring
  :config
  (general-define-key
   :states 'normal
   "C-S-p" 'browse-kill-ring))

(use-package personal
  :straight nil
  :load-path "lib/personal"
  :commands
  (my/open-iterm-here
   my/insert-fullname
   my/send-line-to-iterm)
  :general
  ((normal emacs)
   "C-c K" 'my/nuke-all-buffers
   "C-c s" 'my/split-and-open-shell
   "C-c t" 'my/set-frame-alpha)
  ((insert)
   "C-;" 'my/append-semicolon-to-the-end-of-line)
  ((override)
   "C-c 1" 'comment-region
   "C-c 2" 'uncomment-region
   "<f5>" 'revert-buffer))

(use-package recentf
  :config
  (setq recentf-max-saved-items 50)
  (setq recentf-exclude '("recentf$"
                          "bookmarks$"
                          "mobileorg\.org"
                          "\.org-gcal-token$"
                          "orgtmpcrypt$"
                          "\.emacs-bmk-bmenu-state\.el$"))
  (recentf-mode 1)
  (defvar recentf-save-timer nil)
  (defvar recentf-save-idle-disable-timer nil)
  (defun recentf-save-enable ()
    "Start saving recent files."
    (setq recentf-save-timer
          (run-at-time nil (* 2 60) 'recentf-save-list)
          recentf-save-idle-disable-timer
          (run-with-idle-timer (* 3 60) nil 'recentf-save-disable)))
  (defun recentf-save-disable ()
    "Disable saving recent files."
    (progn
      (cancel-timer recentf-save-timer)
      (setq recentf-save-timer nil)))
  (defun resume-recentf-save ()
    "Resume saving recent files after idle pause."
    (if (eq recentf-save-timer nil)
        (progn
          (message "Re-enable")
          (recentf-save-enable))))
  (add-hook 'focus-in-hook 'resume-recentf-save)
  (recentf-save-enable))

(use-package autorevert :delight auto-revert-mode :config (auto-revert-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package beacon :config (beacon-mode 1))

(use-package which-key :config (which-key-mode) :delight)

(use-package eyebrowse
  :after (evil)
  :config
  (progn
    (set-face-attribute 'eyebrowse-mode-line-active nil :underline t :weight 'bold)
    (eyebrowse-mode t)
    (eyebrowse-setup-opinionated-keys)))

(use-package synonyms
  :commands (synonyms)
  :init
  (setq synonyms-file "~/.emacs.d/synonyms/mthesaur.txt"
        synonyms-cache-file  "~/.emacs.d/synonyms/mthesaur_cache.txt"))

(use-package projectile
  :config
  (progn
    (setq projectile-completion-system 'helm)
    (projectile-mode +1))
  :delight)

(use-package helm-projectile :config (helm-projectile-on))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md$\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package popwin :config (popwin-mode 1))

(use-package alert
  :if (memq window-system '(mac ns))
  :defines (alert-default-style)
  :config
  (setq alert-default-style 'osx-notifier))

(use-package company
  :demand
  :defines (company-dabbrev-downcase)
  :general ("<M-tab>" 'company-complete)
  :config
  (setq company-global-modes '(not org-mode)
        company-dabbrev-downcase nil
        company-tooltip-maximum-width 60)
  (global-company-mode)
  :delight)

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint)
                '(python-pycompile)))
  (setq flycheck-temp-prefix ".flycheck")
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package yasnippet
  :defer 10
  :config
  (yas-global-mode 1)
  :delight yas-minor-mode)

(use-package yasnippet-snippets :after (yasnippet))

(use-package helm-c-yasnippet
  :general ("C-c y" 'helm-yas-complete)
  :after (yasnippet)
  :config
  (setq helm-yas-space-match-any-greedy t))

(use-package avy :general ((override) "C-c SPC" 'avy-goto-char))

(use-package magit
  :general ("C-x g" 'magit-status)
  :config
  (progn
    (general-unbind 'magit-mode-map "M-1" "M-2" "M-3" "M-4")
    (setq magit-section-visibility-indicator nil)))

(use-package evil-magit
  :after (evil magit)
  :init
  (setq
   evil-magit-use-z-for-folds t
   evil-magit-want-horizontal-movement t))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1)
  :delight git-gutter-mode)

(use-package git-timemachine :after (evil) :commands (git-timemachine))

(use-package helm-swoop
  :after (helm)
  :general ("C-s" 'helm-swoop)
  :config
  (progn
    (setq helm-swoop-split-with-multiple-windows t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-pre-input-function (lambda () ""))))

(use-package drag-stuff
  :general
  ("<M-down>" 'drag-stuff-down
   "<M-up>" 'drag-stuff-up)
  :config
  (drag-stuff-global-mode 1)
  :delight)

(use-package all-the-icons)

(use-package neotree
  :after (all-the-icons evil)
  :general
  ("C-c n" 'neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package dash-at-point :commands (dash-at-point dash-at-point-with-docset))

(use-package smartparens
  :config
  (add-hook 'web-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
  :delight)

(use-package bookmark+
  :defines
  (bmkp-bmenu-state-file
   bookmark-default-file
   bmkp-last-as-first-bookmark-file)
  :init
  (setq bmkp-bmenu-state-file
        "~/.emacs.d/.emacs-bmk-bmenu-state.el"
        bookmark-default-file
        "~/.emacs.d/bookmarks"
        bmkp-last-as-first-bookmark-file nil))

(use-package dired+
  :config
  (progn
    (put 'dired-find-alternate-file 'disabled nil)
    (setq dired-use-ls-dired nil
          dired-listing-switches "-alh")))

(use-package all-the-icons-dired
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package hungry-delete :config (global-hungry-delete-mode) :delight)

(use-package aggressive-indent
  :delight
  :config
  (progn
    (global-aggressive-indent-mode 1)
    (setq aggressive-indent-excluded-modes
          (append aggressive-indent-excluded-modes
                  '(coffee-mode
                    python-mode
                    web-mode
                    typescript-mode
                    sass-mode)))))

(use-package expand-region :general ("C-c e" 'er/expand-region))

(use-package edit-indirect)

;;; basics.el ends here
