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
              mac-option-modifier 'meta
              mac-right-option-modifier nil)

(put 'dired-find-alternate-file 'disabled nil)

(windmove-default-keybindings)

(use-package personal
  :load-path "lib/personal"
  :commands
  (my/open-iterm-here
   my/insert-fullname)
  :bind
  (("C-c K" . my/nuke-all-buffers)
   ("C-c p" . my/split-and-open-shell)
   ("C-c t" . my/set-frame-alpha))
  :bind*
  (("C-c 1" . 'comment-region)
   ("C-c 2" . 'uncomment-region)
   ("<f5>" . 'revert-buffer)))

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

(use-package autorevert
  :delight auto-revert-mode)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(use-package which-key
  :config
  (which-key-mode)
  :delight)

(use-package hydra)

(use-package synonyms
  :load-path "site-lisp/synonyms/"
  :commands (synonyms)
  :init
  (setq synonyms-file "~/.emacs.d/synonyms/mthesaur.txt")
  (setq synonyms-cache-file  "~/.emacs.d/synonyms/mthesaur_cache.txt"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package flycheck-rust)

(use-package rust-mode
  :after (flycheck-rust rust-mode)
  :mode ("\\.rs\\'" . rust-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

(use-package popwin
  :config
  (popwin-mode 1))

(use-package alert
  :if (memq window-system '(mac ns))
  :defines (alert-default-style)
  :config
  (setq alert-default-style 'osx-notifier))

(defun my/get-executable-at-dir (bin-dir)
  "Check for executable in parent folders at BIN-DIR."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (exec (and root (expand-file-name bin-dir root))))
    (if (and exec (file-executable-p exec)) exec)))

(defun my/use-eslint-from-node-modules-for-flycheck ()
  "Check parent folders for local eslint."
  (setq-local
   flycheck-javascript-eslint-executable
   (my/get-executable-at-dir "node_modules/eslint/bin/eslint.js")))

(defun my/use-stylint-from-node-modules-for-flycheck ()
  "Check parent folders for local stylint."
  (setq-local
   flycheck-css-stylelint-executable
   (my/get-executable-at-dir "node_modules/stylelint/bin/stylelint.js")))

(defun my/use-flow-from-node-modules-for-flycheck ()
  "Check parent folders for local flow."
  (setq-local
   flycheck-javascript-flow-executable
   (my/get-executable-at-dir "node_modules/flow-bin/vendor/flow")))

(defun my/use-flow-from-node-modules-for-company ()
  "Check parent folders for local flow."
  (setq-local
   company-flow-executable
   (my/get-executable-at-dir "node_modules/flow-bin/vendor/flow")))

(use-package company
  :demand
  :defines (company-dabbrev-downcase)
  :bind ("<M-tab>" . company-complete)
  :config
  (setq company-global-modes '(not org-mode)
        company-dabbrev-downcase nil
        company-tooltip-maximum-width 60)
  (global-company-mode)
  :delight)

(use-package company-flow
  :after (company)
  :config
  (add-to-list 'company-backends 'company-flow)
  (add-hook 'company-mode-hook
            #'my/use-flow-from-node-modules-for-company))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint)
                '(python-pycompile)))
  (setq flycheck-temp-prefix ".flycheck")
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'flycheck-mode-hook
            #'my/use-eslint-from-node-modules-for-flycheck)
  (add-hook 'flycheck-mode-hook
            #'my/use-stylint-from-node-modules-for-flycheck)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package flycheck-flow
  :after (flycheck)
  :config
  (flycheck-add-mode 'javascript-flow 'web-mode)
  (flycheck-add-next-checker 'javascript-flow 'javascript-eslint)
  (add-hook 'flycheck-mode-hook
            #'my/use-flow-from-node-modules-for-flycheck))

(use-package yasnippet
  :defer 7
  :config
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
  (yas-global-mode 1)
  :delight yas-minor-mode)

(use-package helm-c-yasnippet
  :bind ("C-c y" . helm-yas-complete)
  :after (yasnippet)
  :config
  (setq helm-yas-space-match-any-greedy t))

(use-package avy
  :bind* ("C-c SPC" . avy-goto-char))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode 1)
  :delight git-gutter-mode)

(use-package git-timemachine)

(use-package swiper
  :bind ("C-s" . swiper))

(use-package iedit
  :bind ("C-c o" . iedit-mode))

(use-package drag-stuff
  :bind
  (("<M-down>" . drag-stuff-down)
   ("<M-up>" . drag-stuff-up))
  :config
  (drag-stuff-global-mode 1)
  :delight)

(use-package all-the-icons)

(use-package neotree
  :after (all-the-icons)
  :bind ("C-c n" . neotree)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :delight)

(use-package dash-at-point
  :commands (dash-at-point
             dash-at-point-with-docset))

(use-package smartparens
  :config
  (add-hook 'web-mode-hook #'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'lisp-interaction-mode-hook #'smartparens-mode)
  :delight)

(use-package multiple-cursors
  :after (hydra)
  :init
  (defhydra multiple-cursors-hydra (:hint nil)
    "
     ^Up^            ^Down^        ^Miscellaneous^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_q_] Quit"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("q" nil))
  :bind
  (("C-." . mc/mark-next-like-this)
   ("C-," . mc/mark-previous-like-this)
   ("C-c ." . multiple-cursors-hydra/body)))

(use-package bookmark+
  :load-path "site-lisp/bookmark+/"
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
  :load-path "~/.emacs.d/site-lisp/dired+/")

(use-package hungry-delete
  :config
  (global-hungry-delete-mode)
  :delight)

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (setq aggressive-indent-excluded-modes
        (append aggressive-indent-excluded-modes
                '(coffee-mode
                  python-mode
                  web-mode
                  typescript-mode
                  sass-mode)))
  :delight)

(use-package expand-region
  :bind ("C-c e" . er/expand-region))

;;; basics.el ends here
