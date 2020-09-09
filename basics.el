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
 mac-right-option-modifier nil
 mac-pass-command-to-system nil)

(setq-default
 dabbrev-case-fold-search nil
 hippie-expand-try-functions-list
 '(try-expand-dabbrev-visible
   try-expand-dabbrev
   try-expand-dabbrev-from-kill
   try-expand-dabbrev-all-buffers
   try-complete-file-name-partially
   try-complete-file-name
   try-expand-all-abbrevs
   try-expand-list
   try-expand-line))
(general-define-key "M-/" 'hippie-expand)

(windmove-default-keybindings)

(use-package personal
  :straight nil
  :load-path "lib/personal"
  :commands
  (my/open-iterm-here
   my/insert-full-name
   my/send-line-to-iterm
   my/byte-compile-init-files)
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
                          "/straight/build/.*"
                          "/brain/.*.org"
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
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(use-package beacon :config (beacon-mode 1) :delight)

(use-package which-key :config (which-key-mode) :delight)

(use-package smex :config (smex-initialize))

(use-package flx)

(use-package ivy
  :delight
  :after flx
  :general
  ("C-c C-r" 'ivy-resume)
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          ivy-count-format "(%d/%d) "
          ivy-height 15)
    (general-define-key
     :keymaps 'ivy-minibuffer-map
     "C-f" 'ivy-scroll-up-command
     "M-j" 'ivy-next-line
     "M-k" 'ivy-previous-line
     "M-h" 'ivy-backward-delete-char
     "M-l" 'ivy-alt-done
     "M-y" 'ivy-yank-word)))

(use-package ivy-hydra
  :after ivy
  :config
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-l" 'hydra-ivy/body))

(use-package swiper
  :after ivy
  :general ("C-s" 'swiper-isearch))

(use-package counsel
  :after swiper
  :general
  ("M-x" 'counsel-M-x)
  ("C-x C-f" 'counsel-find-file)
  ("C-h b" 'counsel-descbinds)
  ("C-h f" 'counsel-describe-function)
  ("C-h v" 'counsel-describe-variable)
  ("C-x 8 RET" 'counsel-unicode-char)
  :config
  (progn
    (setq counsel-preselect-current-file t
          ivy-initial-inputs-alist
          (remove '(counsel-M-x . "^") ivy-initial-inputs-alist))
    (general-define-key
     :keymaps 'counsel-ag-map
     "M-l" 'ivy-call-and-recenter
     "C-l" 'hydra-ivy/body)))

(use-package ivy-rich
  :after (ivy counsel)
  :config
  (progn
    (ivy-rich-mode 1)
    (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)))

(use-package ripgrep-transient
  :after (counsel transient)
  :straight nil
  :load-path "lib/personal")

(use-package eyebrowse
  :after (evil)
  :config
  (progn
    (set-face-attribute 'eyebrowse-mode-line-active nil :underline t :weight 'bold)
    (eyebrowse-mode t)))

(use-package synonyms
  :commands (synonyms)
  :init
  (setq synonyms-file "~/.emacs.d/synonyms/mthesaur.txt"
        synonyms-cache-file  "~/.emacs.d/synonyms/mthesaur_cache.txt"))

(use-package projectile :config (setq projectile-completion-system 'ivy) :delight)

(use-package counsel-projectile :config (counsel-projectile-mode))

(use-package ibuffer-projectile)

(use-package ibuffer
  :init
  (add-hook
   'ibuffer-hook
   (lambda ()
     (ibuffer-projectile-set-filter-groups)
     (unless (eq ibuffer-sorting-mode 'alphabetic)
       (ibuffer-do-sort-by-alphabetic))))
  :general ("C-x C-b" 'ibuffer))

(use-package ansi-color
  :config
  (progn
    (setq ansi-color-map
          [default bold default italic underline success warning error nil nil
            nil nil nil nil nil nil nil nil nil nil
            nil nil nil nil nil nil nil nil nil nil
            (foreground-color . #1="#2E3440")
            (foreground-color . #2="#BF616A")
            (foreground-color . #3="#A3BE8C")
            (foreground-color . #4="#EBCB8B")
            (foreground-color . #5="#81A1C1")
            (foreground-color . #6="#B48EAD")
            (foreground-color . #7="#8FBCBB")
            (foreground-color . #8="#E5E9F0")
            nil nil
            (background-color . #1#)
            (background-color . #2#)
            (background-color . #3#)
            (background-color . #4#)
            (background-color . #5#)
            (background-color . #6#)
            (background-color . #7#)
            (background-color . #8#)
            nil nil])
    (defun colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min) (point-max))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md$\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package popwin
  :config
  (progn
    (setq popwin:popup-window-height 30)
    (general-define-key "C-c z" popwin:keymap)
    (push "*Kill Ring*" popwin:special-display-config)
    (popwin-mode 1)))

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

(use-package avy :general ((override) "C-c SPC" 'avy-goto-char))

(use-package dumb-jump
  :after ivy
  :general
  ("M-g j" 'xref-find-definitions
   "M-g b" 'xref-pop-marker-stack)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :config
  (setq dumb-jump-selector 'ivy))

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
  (progn
    (setq sp-show-pair-delay 1.2)
    (my/add-to-multiple-hooks
     '(lambda ()
        (progn
          (smartparens-mode)
          (show-smartparens-mode)))
     `(rjsx-mode-hook
       web-mode-hook
       emacs-lisp-mode-hook
       lisp-interaction-mode-hook
       reason-mode-hook)))
  :delight)

(use-package bookmark+
  ;; ensure that org is loaded through means of straight.el
  :after org 
  :straight bookmark-plus
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
  :straight dired-plus
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
                  '(org-mode
                    coffee-mode
                    python-mode
                    web-mode
                    typescript-mode
                    sass-mode
                    rjsx-mode
                    reason-mode)))))

(use-package expand-region :general ("C-c e" 'er/expand-region))

(use-package edit-indirect)

;;; basics.el ends here
