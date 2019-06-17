;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; libvterm Installation Instructions
;;; cd ~/.emacs.d/.vterm && git clone https://github.com/akermu/emacs-libvterm.git
;;; cd ~/.emacs.d/.vterm/emacs-libvterm && git pull origin master
;;; mkdir -p build && cd build && cmake .. && make -j

;;; Code:

(add-to-list 'load-path "~/.emacs.d/.vterm/emacs-libvterm")
(require 'vterm)

(setq
 ansi-color-names-vector
 ["#2E3440" "#BF616A" "#A3BE8C" "#EBCB8B" "#81A1C1" "#B48EAD" "#8FBCBB" "#E5E9F0"])

;;; configure_libvterm.el ends here
