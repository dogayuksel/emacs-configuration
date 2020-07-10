;;; package --- ripgrep transient

;;; Commentary:
;;; Transient interface for ripgrep through counsel-rg.

;;; Code:

(require 'transient)
(require 'counsel)

(transient-define-prefix counsel-rg-transient ()
  "Search with ripgrep."
  :man-page "rg"
  ["Arguments"
   (ripgrep-glob)
   ;; (ripgrep-case)
   ("-l" "Files with matches" ("-l" "--files-with-matches"))
   ("-c" "Count" ("-c" "--count"))
   ("-v" "Invert match" ("-v" "--invert-match"))
   (ripgrep-directory)]
  [["Search"
    ("s" "Ripgrep" counsel-ripgrep)]])

(defclass transient-multi-glob (transient-option) ())

(defun first-match-in-list (re string-list)
  "Return first string in STRING-LIST that match RE.
Return nil when there is no match"
  (cl-find-if
   (lambda (v)
     (and (stringp v)
          (string-match re v)))
   string-list))

;; Parse values from prefix history
(cl-defmethod transient-init-value ((obj transient-multi-glob))
  (when-let
      ;; construct regex from argument (e.g. "--glob=")
      ((re (format "\\`%s.*" (oref obj argument)))
       ;; (oref transient--prefix value) returns a list of
       ;; all the prefix/infix values in the prefix.
       (match (first-match-in-list re (oref transient--prefix value)))
       ;; match will be a string, the part used by the command line for glob
       ;; e.g. "--glob=input-value-1 --glob=input-value-2"
       (list-of-values (if (stringp match) (split-string match " ")))
       ;; parse in values as a list of strings
       ;; e.g.
       ;; ("--glob=input-value-1" "--glob=input-value-2")
       ;; ("input-value-1" "input-value-2")
       (re-inner (format "\\`%s\\(.*\\)" (oref obj argument)))
       (parsed-values (mapcar
                       (lambda (val)
                         (progn
                           (string-match re-inner val)
                           (match-string 1 val)))
                       list-of-values)))
    (oset obj value parsed-values)))

;; Read and parse user input in minibuffer
;; Provide minibuffer with history
(cl-defmethod transient-infix-read ((obj transient-multi-glob))
  (with-slots (value choices) obj
    (let* ((overriding-terminal-local-map nil)
           (prompt (transient-prompt obj))
           (value (mapconcat #'identity value ","))
           (history-key (or (oref obj history-key)
                            (oref obj command)))
           (transient--history (alist-get history-key transient-history))
           (transient--history (if (or (null value)
                                       (eq value "")
                                       (eq value (car transient--history)))
                                   transient--history
                                 (cons value transient--history)))
           (initial-input (and transient-read-with-initial-input
                               (car transient--history)))
           ;; history can be a symbol, which is the history list variable to use
           ;; or it can be a cons cell (HISTVAR . HISTPOS).
           (history (if initial-input
                        (cons 'transient--history 1)
                      'transient--history))
           (value
            ;; Gets a string consisting of
            ;; comma seperated values as input value
            ;; Returns a list of strings
            (completing-read-multiple
             prompt choices nil nil
             (or value initial-input)
             history)))
      (cond ((equal value "") (setq value nil)))
      (when value
        (setf (alist-get history-key transient-history)
              (delete-dups transient--history)))
      value)))

;; Format value stored on inflix object
;; for command line use when transient-args is called
(cl-defmethod transient-infix-value ((obj transient-multi-glob))
  ;; Read value from inflix object, which is a list of strings
  ;; e.g. ("input-value-1" "input-value-2")
  (when-let (values (oref obj value))
    ;; Format list of values the way command line expects
    ;; e.g. "--glob=input-value-1 --glob=input-value-2"
    (mapconcat
     (lambda (value) (format "%s%s" (oref obj argument) value))
     values
     " ")))

;; multi-value inflix arguments by default
;; use completing-read-multiple to parse user input.
;; A function passed into `reader` slot won't be used.
(transient-define-argument ripgrep-glob ()
  :description "Include exclude files"
  :class 'transient-multi-glob
  :key "-g"
  :argument "--glob="
  :prompt "glob(s): "
  :multi-value t)

(transient-define-argument ripgrep-directory ()
  :description "Target Directory"
  :class transient-option
  :key "-d"
  :argument "--directory="
  :prompt "target directory: "
  :reader 'transient-read-directory)

;; (define-infix-argument ripgrep-case ()
;;   :description "Case sensitivity"
;;   :class 'transient-switches
;;   :key "-i"
;;   :argument-format "--%s"
;;   :argument-regexp "\\(--\\(ignore-case\\|case-sensitive\\|smart-case\\)\\)"
;;   :choices '("ignore-case" "case-sensitive" "smart-case"))

(transient-define-suffix counsel-ripgrep (&optional args)
  "counsel-ag do custom ripgrep"
  (interactive (list (transient-args 'counsel-rg-transient)))
  (let*
      ((directory-re "\\`--directory=\\(.*\\)")
       (directory-argument (first-match-in-list directory-re args))
       (directory
        (if (stringp directory-argument)
            (progn
              (string-match directory-re directory-argument)
              (match-string 1 directory-argument))))
       (arguments-without-directory (remove directory-argument args))
       (counsel-ag-extra-args
        (mapconcat 'identity arguments-without-directory " ")))
    (progn
      (counsel-rg nil directory counsel-ag-extra-args))))

(provide 'ripgrep-transient)

;;; ripgrep-transient.el ends here
