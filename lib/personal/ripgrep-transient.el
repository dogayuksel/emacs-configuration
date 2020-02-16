;;; package --- ripgrep transient

;;; Commentary:
;;; Transient interface for ripgrep through helm-ag.

;;; Code:

(require 'transient)
(require 'helm-ag)

(setq helm-ag-base-command "rg --no-heading")

(define-transient-command helm-ag-transient ()
  "Search with ripgrep."
  :man-page "rg"
  ["Arguments"
   (ripgrep-glob)
   (ripgrep-case)
   ("-c" "Count" ("-c" "--count"))
   ("-v" "Invert match" ("-v" "--invert-match"))]
  [["Search"
    ("s" "Choose root" helm-ag-do-ripgrep)
    ("r" "Project root" helm-ag-do-ripgrep-project-root)]])

(defclass transient-multi-value-option (transient-option) ())

(defun first-match-in-list (re string-list)
  "Return first string in STRING-LIST that match RE.
Return nil when there is no match"
  (cl-find-if
   (lambda (v)
     (and (stringp v)
          (string-match re v)))
   string-list))

;; Parse values from history prefix history
(cl-defmethod transient-init-value ((obj transient-multi-value-option))
  (when-let
      ((re (format "\\`%s\\(.*\\)" (oref obj argument)))
       ;; (oref transient--prefix value) returns a list of
       ;; all the prefix/infix values in the prefix.
       (match (first-match-in-list re (oref transient--prefix value) ))
       ;; match will be a string used by the command line
       ;; e.g. "--glob:input-value-1 --glob:input-value-2"
       (list-of-values (if (stringp match) (split-string match " ") ))
       ;; parse in values as a list of strings
       ;; e.g. ("input-value-1" "input-value-2")
       (parsed-values (mapcar
                       (lambda (val)
                         (progn
                           (string-match re val)
                           (match-string 1 val)))
                       list-of-values)))
    (oset obj value parsed-values)))

;; Read and parse user input in minibuffer
;; Provide minibuffer with history
(cl-defmethod transient-infix-read ((obj transient-multi-value-option))
  (with-slots (value) obj
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
             prompt nil nil nil
             (or value initial-input)
             history)))
      (cond ((equal value "") (setq value nil)))
      (when value
        (setf (alist-get history-key transient-history)
              (delete-dups transient--history)))
      value)))

;; Format value stored on inflix object
;; for command line use when transient-args is called
(cl-defmethod transient-infix-value ((obj transient-multi-value-option))
  ;; Read value from inflix object, which is a list of strings
  ;; e.g. ("input-value-1" "input-value-2")
  (when-let (values (oref obj value))
    ;; Format list of values the way command line expects
    ;; e.g. "--glob:input-value-1 --glob:input-value-2"
    (mapconcat
     (lambda (value) (format "%s%s" (oref obj argument) value))
     values
     " ")))

;; multi-value inflix arguments by default
;; use completing-read-multiple to parse user input.
;; A function passed into `reader` slot won't be used.
(define-infix-argument ripgrep-glob ()
  :description "Include exclude files"
  :class 'transient-multi-value-option
  :key "-g"
  :argument "--glob="
  :prompt "glob(s): "
  :multi-value t)

(define-infix-argument ripgrep-case ()
  :description "Case sensitivity"
  :class 'transient-switches
  :key "-i"
  :argument-format "--%s"
  :argument-regexp "\\(--\\(ignore-case\\|case-sensitive\\|smart-case\\)\\)"
  :choices '("ignore-case" "case-sensitive" "smart-case"))

(define-suffix-command helm-ag-do-ripgrep (&optional args)
  "Helm-ag do custom ripgrep"
  (interactive (list (transient-args 'helm-ag-transient)))
  (setq helm-ag--extra-options (mapconcat 'identity args " "))
  (helm-do-ag))

(define-suffix-command helm-ag-do-ripgrep-project-root (&optional args)
  "Helm-ag do custom ripgrep"
  (interactive (list (transient-args 'helm-ag-transient)))
  (setq helm-ag--extra-options (mapconcat 'identity args " "))
  (helm-do-ag-project-root))

(provide 'ripgrep-transient)

;;; ripgrep-transient.el ends here
