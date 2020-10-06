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
   evil-want-keybinding nil
   evil-respect-visual-line-mode t)
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
  (evil-collection-want-unimpaired-p nil)
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
           (evil-define-key 'normal evil-mc-key-map (kbd x) nil)
           (evil-define-key 'visual evil-mc-key-map (kbd x) nil)))
     '("C-p" "C-n" "C-t" "M-p" "M-n"))
    (defhydra evil-mc-hydra (:color red :hint nil)
      "
    Multiple Cursors: %(evil-mc-get-cursor-count)
  ^^^^^^----------------------------------------------------------------
    _m_ Make all Cursors   _u_ Undo all Cursors   _U_ Undo last Cursor
    _s_ Pause Cursors      _r_ Resume Cursors

      ^^          first _f_
        _k_             _p_                  _P_
      ^LINE^         CURSOR _h_ here       ^MATCH
        _j_             _n_                  _N_
      ^^           last _l_             skip _S_
    "
      ("m" evil-mc-make-all-cursors)
      ("u" evil-mc-undo-all-cursors :color blue)
      ("U" evil-mc-undo-last-added-cursor)
      ("s" evil-mc-pause-cursors)
      ("r" evil-mc-resume-cursors)
      ("f" evil-mc-make-and-goto-first-cursor)
      ("l" evil-mc-make-and-goto-last-cursor)
      ("h" evil-mc-make-cursor-here)
      ("j" evil-mc-make-cursor-move-next-line)
      ("k" evil-mc-make-cursor-move-prev-line)
      ("n" evil-mc-make-and-goto-next-cursor)
      ("p" evil-mc-make-and-goto-prev-cursor)
      ("S" evil-mc-skip-and-goto-next-match)
      ("N" evil-mc-make-and-goto-next-match)
      ("P" evil-mc-make-and-goto-prev-match))))

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
