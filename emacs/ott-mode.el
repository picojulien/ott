;; Indentation

;; section begining are not indented
(defcustom default-indent-size 2 "default indentation increment" )

(defcustom ott-section-indent 0 "Indentation of section begining")
(defcustom ott-after-section-indent (+ ott-section-indent default-indent-size) "Indentation of section begining")
(defcustom ott-section-start-keywords
  '("metavar" "indexvar" "grammar" "substitutions"
   "freevars" "defns" "funs" "terminals" "formula" "parsing" )
  "Ott section keywords"
  )


(defcustom ott-subsection-indent (+ ott-section-indent default-indent-size)  "Indentation of subsection begining")
(defcustom ott-subsection-start-keywords
  '("subrules" "single" "multiple" "defn" "fun")
  "Ott subsection keywords"
  )

(defcustom ott-indent-comments nil "Shall we indent comments")

(defun is-section-start-line ()
  "return true if the previously matched line start with a section keyword"
  (seq-contains  ott-section-start-keywords (match-string 1))
  )


(defun is-subsection-start-line ()
  "return true if the previously matched line start with a subsection keyword"
  (seq-contains  ott-subsection-start-keywords (match-string 1))
  )

(defun is-header ()
  "return true if the previously matched line start with a subsection keyword"
  (beginning-of-line)
  (if (looking-at "[ \t]*\\([A-Za-z]+\\)" )
      (or
       (is-section-start-line)
       (is-subsection-start-line))
    )
  )

(defun is-comment ()
  "return true if the current line is a comment"
  (save-excursion
    (beginning-of-line)
    (looking-at "[ \t]*%")
    )
  )

(defun non-empty-line ()
  "return true if the line is not entirely made of space"
  (beginning-of-line)
  (looking-at "[ \t]*\\([^[:space:]\n\r]+\\)")
  )

(defun is-bar ()
  (beginning-of-line)
  (looking-at "[ \t]*|")
  )

(defun ott-indent-line ()
  "Indent current line of ott code"
  (if(and (is-comment) (not ott-indent-comments)) ; comments are indented only if ott-indent-comments is set
      ()
    (progn
      (beginning-of-line)
      (if(bobp)  ;first line of the file
          (indent-line-to 0)
        (let ((not-indented t) current-indent)
          (if (is-bar)                      ; | bar
              (save-excursion
                (while not-indented
                  (forward-line -1)
                  (cond
                   ((is-bar)                ; align on previous bar
                                        ;(message "ott-indent-line : bar indented  relatively to bar at %s" (what-line))
                    (setq current-indent (current-indentation))
                    (setq not-indented nil))
                   ((non-empty-line)        ; shift from previous text
                                        ;(message "ott-indent-line : bar indented  relatively to text at %s" (what-line))
                    (setq current-indent (+ (current-indentation) default-indent-size))
                    (setq not-indented nil))
                   ((bobp)                  ;align at zero if no previous text
                                        ;(message "ott-indent-line : bar indented  on first line")
                    (setq current-indent ott-section-indent)
                    (setq not-indented nil))
                   ))))
          (if (non-empty-line )
              (cond
               ((is-section-start-line)       ;section start at 0
                                        ;(message "ott-indent-line : non-empty line indented as section")
                (setq current-indent ott-section-indent)
                )
               ((is-subsection-start-line)    ;subsection start at one tab
                                        ;(message "ott-indent-line : non-empty line indented as sub-section")
                (setq current-indent ott-subsection-indent)
                )
               ( t                            ;default : not header, not bar
                 (save-excursion
                   (while not-indented
                     (forward-line -1)
                     (cond
                      ((is-header)              ;text is shifted from header
                                        ;(message "ott-indent-line : non-empty indented relatively to header at %s" (what-line))
                       (setq current-indent (+ (current-indentation) default-indent-size))
                       (setq not-indented nil))
                      ((is-bar)         ;text is unshifted from previous bar
                                        ;(message "ott-indent-line : non-empty indented relatively to bar at %s" (what-line))
                       (setq current-indent (- (current-indentation) default-indent-size))
                       (setq not-indented nil))
                      ((and (non-empty-line) (not (is-comment))) ;text is aligned with previous non-empty, non-header/comment line
                                        ;(message "ott-indent-line : non-empty indented relatively to text at %s" (what-line))
                       (setq current-indent (current-indentation))
                       (setq not-indented nil))
                      ((bobp)                   ;align at zero if no previous text
                                        ;(message "ott-indent-line : non-empty indented on first line")
                       (setq current-indent 0)
                       (setq not-indented nil))
                      )))))
            (setq current-indent 0)       ;empty line are indented at 0
            )
          (indent-line-to current-indent)
          )
        )
      )
    )
  )
(defun indent-at-point ()
  "interactively use ott-indent-line (debug purpose)"
  (interactive)
  (ott-indent-line)
  )


(define-generic-mode 'ott-mode
  ;; comments:
  '("%")
  ;; keywords:
  '("metavar" "indexvar" "grammar" "embed" "subrules" "contextrules"
    "substitutions" "single" "multiple" "freevars" "defns" "defn" "by"
    "homs" "funs" "fun" "parsing" "begincoqsection" "endcoqsection"
    "coqvariable" "left" "right" "terminals" "formula" "judgement")
  ;; more font-lock
  '(
;    ("\\[\\[.+\\]\\]" . 'font-lock-warning-face)
;    ("\\[\\[[^\\]]+\\]\\]" . 'font-lock-warning-face)
    ("{{\\([^{}]+{[^{}]*}\\)*[^{}]*}}" . 'font-lock-doc-face)
;    ("{{[^}]+}}" . 'font-lock-doc-face)
    ("(\\+.+\\+)" . 'font-lock-keyword-face)
    ("<<.*" . 'font-lock-keyword-face)
    (">>" . 'font-lock-keyword-face)
    ("</" . 'font-lock-keyword-face)
    ("//" . 'font-lock-keyword-face)
    ("IN" . 'font-lock-keyword-face)
    ("/>" . 'font-lock-keyword-face)
    (" | " . 'font-lock-keyword-face)
    (";" . 'font-lock-keyword-face)
    ("<::" . 'font-lock-keyword-face)
    ("_::" . 'font-lock-keyword-face)
    ("::=" . 'font-lock-keyword-face)
    ("::" . 'font-lock-keyword-face)
    ("<=" . 'font-lock-keyword-face)
    )
  ;; auto-mode-alist
  (list "\\.ott\\'")
  ;; Pre mode-hook (setting indentation) 
  ( list
    (lambda () (set (make-local-variable 'indent-line-function) 'ott-indent-line))
    (lambda () (set (make-local-variable 'tab-always-indent) t))
    )
  "Major mode for editing ott format files.")


;    ("\\%.*" . 'font-lock-doc-face)

(provide 'ott-mode)
