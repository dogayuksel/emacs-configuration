;;; .emacs --- My emacs configuration file

;;; Commentary:
;;; powerline modifications based on airline

;;; Code:

(use-package powerline
  :config
  (progn
    (powerline-default-theme)
    (setq-default powerline-display-mule-info nil
                  powerline-display-hud t)))

(use-package airline-themes
  :config
  (progn
    (load-theme 'airline-badwolf)
    (setq airline-utf-glyph-separator-left      #xe0b0
          airline-utf-glyph-separator-right     #xe0b2
          airline-utf-glyph-subseparator-left   #xe0b1
          airline-utf-glyph-subseparator-right  #xe0b3
          airline-utf-glyph-branch              #xe0a0
          airline-utf-glyph-readonly            #xe0a2
          airline-utf-glyph-linenumber          #xe0a1)))

(diminish 'auto-revert-mode)
(diminish 'visual-line-mode)
(diminish 'buffer-face-mode)
(diminish 'auto-revert-mode)

(setq-default
 mode-line-format
 (quote
  ("%e"
   (:eval
    (let*
        ((active
          (powerline-selected-window-active))
         (separator-left
          (intern
           (format "powerline-%s-%s"
                   (powerline-current-separator)
                   (car powerline-default-separator-dir))))
         (separator-right
          (intern
           (format "powerline-%s-%s"
                   (powerline-current-separator)
                   (cdr powerline-default-separator-dir))))
         (mode-line-face
          (if active 'mode-line 'mode-line-inactive))
         (outer-face
          (if
              (powerline-selected-window-active)
              'airline-normal-outer
            'powerline-inactive1))
         (inner-face
          (if
              (powerline-selected-window-active)
              'airline-normal-inner
            'powerline-inactive2))
         (center-face
          (if
              (powerline-selected-window-active)
              'airline-normal-center
            'powerline-inactive1))
         (lhs-mode
          (list
           (powerline-raw "%*" outer-face 'l)
           (powerline-raw " " outer-face)
           (funcall separator-left outer-face inner-face)))
         (lhs-rest
          (list
           (powerline-raw (airline-get-vc) inner-face)
           (powerline-raw " " inner-face)
           (funcall separator-left inner-face center-face)
           (powerline-raw mode-line-misc-info center-face 'l)
           (powerline-raw
            (airline-shorten-directory
             default-directory airline-shortened-directory-length)
            center-face 'l)
           (powerline-raw "%b" center-face)
           (when
               (and
                (boundp 'which-func-mode)
                which-func-mode)
             (powerline-raw which-func-format center-face 'l))
           (when
               (boundp 'erc-modified-channels-object)
             (powerline-raw erc-modified-channels-object center-face 'l))))
         (lhs
          (append lhs-mode lhs-rest))
         (rhs
          (list
           (powerline-raw global-mode-string center-face 'r)
           (powerline-major-mode center-face 'l)
           (powerline-process center-face)
           (powerline-raw " " center-face)
           (funcall separator-right center-face inner-face)
           (when powerline-display-buffer-size
             (powerline-buffer-size inner-face 'l))
           (when powerline-display-mule-info
             (powerline-raw mode-line-mule-info inner-face 'l))
           (powerline-raw " " inner-face)
           (funcall separator-right inner-face outer-face)
           (powerline-raw "%l" outer-face 'l)
           (powerline-raw ":" outer-face)
           (powerline-raw "%c" outer-face 'r)
           (if
               (string-equal (format-mode-line "%p") "Bottom")
               (powerline-raw "Bot" outer-face 'r)
             (powerline-raw "%p" outer-face 'r))
           (when powerline-display-hud
             (powerline-hud inner-face outer-face)))))
      (concat
       (powerline-render lhs)
       (powerline-fill center-face (powerline-width rhs))
       (powerline-render rhs)))))))

;;; my-modeline.el ends here
