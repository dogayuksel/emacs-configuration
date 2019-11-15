;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; Evil packages

;;; Code:

;; setup evil interface

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
  :after (undo-tree)
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
  :straight
  (:type git :host github :repo "emacs-evil/evil-collection")
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

(use-package evil-surround :after (evil) :config (global-evil-surround-mode 1))

(use-package evil-mc
  :after (hydra evil)
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

(use-package evil-escape
  :after (evil)
  :config
  (progn
    (setq-default evil-escape-key-sequence "fd")
    (setq-default evil-escape-delay 0.2)
    (evil-escape-mode))
  :delight)

(use-package hydra)
(use-package transient)
(use-package matcha
  :after (hydra transient evil)
  :straight
  (:type git :host github :repo "jojojames/matcha"
         :fork (:host github :repo "dogayuksel/matcha"))
  :config
  (progn
    (matcha-setup)
    (evil-declare-not-repeat 'matcha-me-space)))

;;; evil.el ends here
