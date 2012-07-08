;;; occam-mode.el --- OCCAM2 mode for GNU emacs

;; Copyright (C) Jesper Larsson Traff and DIKU

;; Authors: Jesper Larsson Traff
;;          Jared C. Saia <saia@itl.atr.co.jp>
;;          Phil Nitschke <phil@mrad.com.au>
;; Maintainer: none
;; Keywords: occam, languages

;; LCD Archive Entry:  (PCN: not sure what all this means...)
;; occam-mode|Phil Nitschke|phil@mrad.com.au|
;; OCCAM programing mode.|
;; 20-Feb-1997||~/modes/occam-mode.el.Z|

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Just plug it in and run.  Requires no modification.
;; TAB and CR automatically indents.
;; All OCCAM keywords (which are separated into process keywords (which force
;; indentation) and reserved words) are recognised and uppercase'd.

;;; Install:

;; Stick something like this in your `.emacs' file:

;(autoload 'occam-mode "occam-mode" "Major mode for Occam" t)
;; Set up to recognise our files:
;(setq auto-mode-alist (cons '("\\.occ$" . occam-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.inc$" . occam-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\\.pgm$" . occam-mode) auto-mode-alist))

;; Maybe these hooks would be good too:

;(defun enable-horizontal-scroll ()
;  "The `auto-show' package is used to horizontally scroll the window
;  when the point moves off the left or right side."
;  (require 'auto-show)
;  (setq truncate-lines t))
;(add-hook 'occam-mode-hook 'enable-horizontal-scroll)

;(add-hook 'occam-mode-hook 'my-imenu-menubar)
;(defun my-imenu-menubar ()
;  "Function for adding imenu list of known functions."
;  (imenu-add-to-menubar "IMenu"))

;; If your Emacs version supports it:
;(global-font-lock-mode t)

;;; Change Log:

;; February 1997; Phil Nitschke
;;   - Added more reserved words/keywords
;;   - Added IMenu support (to make it work, you need at least GNU Emacs 19.31)
;;   - Fixed `occam-comment-indent'
;;   - Fixed and enhanced hilit19 support (now everything is coloured!)
;;   - Added font-lock support (better than hilit19)
;;   - other minor fixes
;; Autumn 1993; Jared Saia
;;   - automatically add colons to declarations if necessary
;;   - added more key words
;;   - cleaned up the indentation scheme.
;;   - added the function occam-move-region which indents/unindents a block of
;;     text the number of spaces specified by a prefix argument
;;   - added feature to show the keyword in the current scope(similar to
;;     blink matching paren) when backspacing
;;   - added hilit19 functionality to occam mode (auto highlight of keywords, 
;;     etc.) requires emacs 19- to use this feature
;; Autumn 1989; Jesper Larsson Traff
;;   - original

;;; Code:

;;any expression which evaluates to a truth value
(defconst if-reg-exp "[^:\n]*\\(>\\|<\\|TRUE\\|FALSE\\|[^:]=\\|AND\\|OR\\|NOT\\).*")

;; regular expression to determine whether to unindent after a case
;; keyword
(defconst case-reg-exp "\\([^\n]*;[^\n]*\\|[ \t]*[a-zA-Z1-9\\.]+\\([ \t]*--[^\n]*\\|[ \t]*$\\)\\)")

(defconst alt-reg-exp "[^\n]* \\? [^\n]*")

(defconst occam-indent 2  
  "*OCCAM standard indentation (do not change!)")

;; words which signal that a colon is needed at the end of this line.
(defconst col-words
  '("CHAN"
    "PLACE"
    "PORT"
    "["
    "VAL"
    "BOOL"
    "TIMER"
    "INT" "INT16" "INT32" "INT64" ; integer declarations
    "REAL" "REAL32" "REAL64"      ; real declarations
    "BYTE"))

(defconst occam-process-keywords
  '("SEQ"                         ; sequential process
    "PAR"                         ; parallel process
    "IF"                          ; conditional process
    "ALT"                         ; alternative (special) process
    "WHILE"                       ; iterative process
    "CASE"                        ; selection process
    "VALOF"
    "PROC"
    "PROCESSOR"
    "PLACED"
    "PRI"
    "RESULT"
    )
  "*OCCAM proccess keywords")

(defconst occam-reserved-words
  '("INT" "INT16" "INT32" "INT64" ; integer declarations
    "REAL" "REAL32" "REAL64"      ; real declarations
    "BYTE"                        ; byte (character) declaration
    "BOOL" "TRUE" "FALSE"         ; boolean declaration and constants
    "AFTER"
    "AND"
    "AT"
    "BITAND"
    "BITNOT"
    "BITOR"    
    "CHAN"                        ; channel declaration
    "FOR"                         ; replicator keyword
    "FROM"                        ; array selector keyword
    "INCLUDE"
    "IS"
    "MAPPING" "MAP" "SET" "ONTO" "DO" "CONFIG"  ; configuration mapping files
    "MINUS" "PLUS" "TIMES" "REM"
    "MOSTNEG"
    "MOSTPOS"
    "NETWORK" "NODE" "EDGE" "CONNECT" "WITH" "HOST" "ARC" ; H/W descriptions
    "NOT"
    "OF"
    "OR"
    "PLACE"
    "PORT"
    "PROTOCOL"                    ; protocol declaration
    "RETYPES"
    "ROUND" "TRUNC"
    "SIZE"                        ; size operator
    "SKIP" "STOP"                 ; special processes
    "TIMER"                       ; timer declaration
    "USE"
    "VAL"
    )
  "*OCCAM reserved words (will be capitalized)")

(defvar occam-mode-syntax-table nil
  "Syntax table in use in OCCAM mode buffers")

(if occam-mode-syntax-table
    ()
  (setq occam-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\? "." occam-mode-syntax-table)
  (modify-syntax-entry ?\! "." occam-mode-syntax-table)
  (modify-syntax-entry ?\: "." occam-mode-syntax-table)
  ;;(modify-syntax-entry ?. "w" occam-mode-syntax-table)
  (modify-syntax-entry ?\\ "\\" occam-mode-syntax-table)
  (modify-syntax-entry ?/ "." occam-mode-syntax-table)
  (modify-syntax-entry ?* "." occam-mode-syntax-table)
  (modify-syntax-entry ?+ "." occam-mode-syntax-table)
  (modify-syntax-entry ?- "." occam-mode-syntax-table)
  (modify-syntax-entry ?= "." occam-mode-syntax-table)
  (modify-syntax-entry ?< "." occam-mode-syntax-table)
  (modify-syntax-entry ?> "." occam-mode-syntax-table)
  (modify-syntax-entry ?& "." occam-mode-syntax-table)
  (modify-syntax-entry ?| "." occam-mode-syntax-table)
  (modify-syntax-entry ?~ "." occam-mode-syntax-table)
  ;;comment start is the string "--"
  (modify-syntax-entry ?- "< 12" occam-mode-syntax-table)
  ;comment end is return
  (modify-syntax-entry ?\n ">" occam-mode-syntax-table)
  (modify-syntax-entry ?\t " " occam-mode-syntax-table)
  (modify-syntax-entry ?  " " occam-mode-syntax-table)
  (modify-syntax-entry ?\' "\"" occam-mode-syntax-table))

(defvar occam-mode-map ()
  "Keymap used in OCCAM mode")

(if occam-mode-map
    ()
  (setq occam-mode-map (make-sparse-keymap))
  (define-key occam-mode-map " " 'uppercase-occam-keyword)
  ;;inserts a colon and indents correctly if needed
  (define-key occam-mode-map ":" 'occam-insert-col)
  (define-key occam-mode-map "\r" 'occam-indent-newline)
  ;;(define-key occam-mode-map "\t" 'indent-according-to-mode)
  (define-key occam-mode-map "" (quote occam-comment-region))
  (define-key occam-mode-map "m" (quote occam-move-region))
  (define-key occam-mode-map "\177" 'backward-delete-unindent))

(defvar occam-imenu-generic-expression
  '(
    (nil 
     "^\\([ \t]\\)*\\(INLINE \\)*\\(PROC\\|FUNCTION\\)\\([^(]*\\)" 4)
    ;("Variables" 
    ; "\\(^[ \t]*\\|([ \t]*\\)\\(VAL \\)*\\(\\[[]a-zA-Z0-9 \t.]+\\]\\)*\\(CHAN OF[ \t]+[^ \t]*\\|PORT OF[ \t]+[^ \t]*\\|\\bPLACE\\b\\|\\bBOOL\\b\\|\\bBYTE\\b\\|\\bTIMER\\b\\|\\bINT\\b\\|\\bINT16\\b\\|\\bINT32\\b\\|\\bINT64\\b\\|\\bREAL\\b\\|\\bREAL32\\b\\|\\bREAL64\\b\\)\\([^:),;
;]+\\)[:),;]" 5)
    )
  "Imenu generic expression for Occam mode.  See `imenu-generic-expression'.")

;;;###autoload
(defun occam-mode ()
  "Major mode for editing OCCAM programs.
TAB and CR automatically indents.
All OCCAM keywords (which are separated into process keywords which force  
indentation and reserved words) are recognised and uppercase'd.

Variables and constants controlling case change:
    occam-indentation :      indentation, default 2
    occam-process-keywords : list of process keywords
    occam-reserved-words :   list of reserved words

The value of the variable occam-mode-hook (must be a function name) is called  
with no arguments prior to entering  OCCAM mode if the value of that variable
is non-nil"
  (interactive)
  (kill-all-local-variables)
  (use-local-map occam-mode-map)
  (setq mode-name "OCCAM")
  (setq major-mode 'occam-mode)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'my-indent)
  ;;take care of commenting regions
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "--+ *")
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'occam-comment-indent)
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'last-keyword-data)
  (setq last-keyword-data (get-last-keyword)) ; PCN 9-Jan-96
  (make-local-variable 'font-lock-keywords)
  ;; Font lock support
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(occam-font-lock-keywords nil nil))
  ;; Imenu support
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression occam-imenu-generic-expression)
  (run-hooks 'occam-mode-hook))

(defun occam-indent-line (indent-level)
  "Indents current OCCAM line"
  (save-excursion
    (beginning-of-line)
    (let ((p (point)))
      (insert-char 32 indent-level)
       ;;(indent-to indent-level)
      ;;delete any white space user may have entered
      (while (looking-at "[ \t]")
        (delete-char 1))
      (untabify p (point)))))

;;if there is a keyword on the current line returns that word
;;otherwise return null
(defun get-key-word-this-line()
  ;;first check to see if the word we are looking at is a keyword
  (save-excursion
    (let ((poss (current-word)))
      ;;make sure the word is not in a comment or this is a blank line
      (if (looking-at "\\([ \t]*$\\)\\|\\([ \t]*--\\)")
          nil
        (if (word-in-list poss occam-process-keywords)
            poss
          (progn
            ;;first check to see if a function
            (forward-word 1)
            (setq poss (current-word))
            (if
                (string-equal poss "FUNCTION")
                poss
              ;;now check the end of the line to see if case is there
              (end-of-line)
              (forward-word -1)
              (setq poss (current-word))
              (if
                  (string-equal poss "CASE")
                  poss
                nil))))))))

;; returns a dotted pair of the keyword and info as to whether or not it is
;; a new-word (immediately proceeding) or an old world (further back)
;; info concerning whether it may need to be unindented or whether it
;; there is no chance of unindenting

(defun get-last-keyword ()
  (interactive)
  (save-excursion
    (let ((keyword nil)(done nil)(current-indent-level)(last-indent-level))
      (beginning-of-line)
      (back-to-indentation)
      (setq current-indent-level (current-column))
      ;;first check to see if this line has a keyword in it
      (setq keyword (get-key-word-this-line))
      (if keyword (list 'new-word current-indent-level keyword)
        (while (and (null keyword) (not done))
          (setq last-indent-level (current-column))
          ;;skip everything which is indented more than the last command
          (if (< last-indent-level current-indent-level)
              (setq keyword (get-key-word-this-line)))
          (if (bobp) (setq done 't)
            (beginning-of-line 0)
            (back-to-indentation)))
        (list 'old-word last-indent-level keyword)))))

(defun calculate-occam-indent ()
  "calculate indentation for current OCCAM line"
  (interactive)
    (save-excursion
      (setq last-keyword-data (get-last-keyword))
      (let ((last-keyword-type (car last-keyword-data))
            (indentation (car (cdr last-keyword-data)))
            (last-keyword (car (cdr (cdr last-keyword-data)))))
        (cond
         ((eq last-keyword-type 'new-word)
          ;;indent 2 spaces from last no matter what the keyword is
          (+ 2 indentation))
         ((string-equal last-keyword "IF")
          ;;indent 4 spaces / possibly remove 2 spaces when the syntax
          ;;of the current line is read
          (+ 4 indentation))
         ((string-equal last-keyword "CASE")
          (+ 4 indentation))
         ((string-equal last-keyword "ALT")
          (+ 4 indentation))
         ((null last-keyword) 0)
         (t (+ 2 indentation))
         ))))

(defun looking-at-word-in-list-p(list)
  (save-excursion
    (let (bow eow)
      (progn
        (beginning-of-line)
        (word-in-list (current-word)
                      list)))))

(defun current-word()
  (save-excursion
    (let ((b))
      (forward-word 1)
      (setq b (point))
      (forward-word -1)
      (buffer-substring b (point)))))

(defun uppercase-occam-keyword ()
  "check if last word was an OCCAM keyword"
  (interactive)
  (occam-keyword (append occam-process-keywords occam-reserved-words))
    (insert " "))

(defun occam-insert-col()
  (interactive)
  (let ((nulllineflag nil)(eolflag nil))
    (if (eolp)
        (progn  
          (setq eolflag 't)
          (save-excursion
            (beginning-of-line)
            (if (looking-at "[ \t]*$")
                (setq nulllineflag 't)))))
    (if
        (and eolflag nulllineflag)
        (progn
          (kill-line 0)
          (insert ":\n\n"))
      (if eolflag
          (progn
            (end-of-line)
            (insert ":"))
        (insert ":")))
))
     

(defun occam-move-region (arg)
  "Indents a marked block by ARG spaces, similar to `indent-rigidly'."
  (interactive "P")
  (if (< (point) (mark))
      (beginning-of-line)
    (exchange-point-and-mark)
    (beginning-of-line))
  (save-excursion
    (save-restriction
      (let ((a (prefix-numeric-value arg)))
        (call-interactively 'narrow-to-region)
        (goto-char (point-min))
        (while (not (eobp))
          (beginning-of-line)
          (if (> a 0)
              (insert-char 32 a)
            (delete-char (- a)))
          (beginning-of-line 2))))))

(defun occam-comment-indent ()
  (save-excursion
    (let* ((empty-line (save-excursion (beginning-of-line)
				       (looking-at "[ \t]*$")))
	   (comment-on-line (save-excursion (beginning-of-line)
				       (looking-at "[ \t]*--+.*$"))))
      (if (or empty-line comment-on-line)
	  ;; indent to the same level as the line directly above this line
	  ;; so this function just returns the amount the above line
	  ;; is indented to.
	  (progn (previous-line 1)
		 (beginning-of-line)
		 (skip-chars-forward " \t")
		 (if (looking-at-word-in-list-p occam-process-keywords)
		     (+ 2 (current-column))
		   (current-column)))
	;; looking at line with some code in it
	(let* ((eolpos (save-excursion (end-of-line) (point))))
	  (beginning-of-line)
	  (if (re-search-forward comment-start-skip eolpos 'move)
	      (progn (beginning-of-line)
		     (search-forward comment-start)
		     ;; go back to start of comment chars
		     (goto-char (match-beginning 0))
		     ;; go back to end of code before this comment
		     (skip-syntax-backward " "))
	    (end-of-line)
	    ;; go back to end of code before this comment
	    (skip-syntax-backward " "))
	  (max (if (bolp) 0 (1+ (current-column)))
	       comment-column))))))

;;if given an arg uncomment the region
;;if no arg, comment the region

(defun occam-comment-region (&optional arg)
  (interactive "P")
  (if (< (point) (mark))
      (beginning-of-line)
    (exchange-point-and-mark)
    (beginning-of-line))
  (cond ((null arg)
         (save-excursion
           (save-restriction
             (call-interactively 'narrow-to-region)
             (goto-char (point-min))
             (skip-chars-forward " \t")
             (let ((col (current-column)))
               (beginning-of-line)
               (while (not (eobp))
                 (indent-to  col)
                 (insert "--")
                 ;;delete any white space usepr may have entered
                 (beginning-of-line 2))))))
        ((equal (prefix-numeric-value arg) 1)
         (save-excursion
           (save-restriction
             (call-interactively 'narrow-to-region)
             (goto-char (point-min))
             (while (re-search-forward "^[ \t]*--" nil t)
               (replace-match "" nil nil)
               (beginning-of-line 2)))))
        ((equal (prefix-numeric-value arg) 4)
         (save-excursion
           (save-restriction
             (call-interactively 'narrow-to-region)
             (goto-char (point-min))
             (while (re-search-forward "^[ \t]*--" nil t)
               (replace-match "" nil nil)
               ))))
        ))

(defun my-indent ()
  (interactive)
  ;;delete any white space the user may have typed in
  (let ((ind-level 0))
    (end-of-line 0)
    (setq ind-level (calculate-occam-indent))
    (beginning-of-line 2)
    (while (looking-at "[ \t]")
      (delete-char 1))
    (occam-indent-line ind-level)
    (while (looking-at "[ \t]")
      (forward-char 1))))


(defun occam-indent-newline ()
  "In default case indent new line to current indentation but:
   1) if this line contained a process keyword -> indent 2 spaces for the next line
   2) if this line is an expression and not right after if/case/alt  
      -> unindent this line 2 spaces.
   3) if this line is after if/case/alt -> indent 2 spaces"
  (interactive)
  (if (not (eolp))
      (let ((ind (calculate-occam-indent)))
        (newline)
        (occam-indent-line ind)
        (while (looking-at "[ \t]")
          (forward-char 1))
        )
  ;;uppercase the word if it is a keyword
    (occam-keyword (append occam-process-keywords occam-reserved-words))
    (let ((needed-punc nil) (current-indent-level 0)  
          (last-keyword-type (car last-keyword-data))
          (next-line-indent-level (calculate-occam-indent))  
          (keyword-type (car last-keyword-data))
          (keyword-indent-level (car (cdr last-keyword-data)))
          (last-keyword (car (cdr (cdr last-keyword-data)))))
      (save-excursion
        (let ((eol (point))(eow nil)(bow nil))
          (beginning-of-line)
          (if (or (looking-at ".*--") (looking-at "[ \t]*$"))  
              (setq needed-punc 'none))
          (back-to-indentation)
          (setq current-indent-level (current-column))
          (forward-word 1)
          (setq eow (point))
          (forward-word -1)
          (setq bow (point))
          (cond  
           ((eq needed-punc 'none))
           ((string= (buffer-substring bow eow) "PROC")
            (setq needed-punc 'parens))
           ((word-in-list (upcase (buffer-substring bow eow))
                          col-words)
            (setq needed-punc 'colon))
           ;; check to see if this line should be unindented
           ;; this is true iff the last keyword was an if case or
           ;; alt *and* this line matches the appropriate regexp.
           ((and
             (= (- current-indent-level keyword-indent-level) 4)
             ;;neither of the last two keywords(this line or the line
             ;;before can be new
             (eq keyword-type 'old-word)
             (eq last-keyword-type 'old-word)
             (or
              (and (string-equal last-keyword "IF")
                   (looking-at if-reg-exp))
              (and (string-equal last-keyword "CASE")
                   (looking-at case-reg-exp))
              (and (string-equal last-keyword "ALT")
                   (looking-at alt-reg-exp))))
            (setq needed-punc 'unindent)))
          (if (looking-at "[ \t]*$")
              (delete-region (point) eol)
            ())))
      ;;add a ":" if necessary
      (if (and
           (eq needed-punc 'colon)
           (not (string-equal ":" (buffer-substring (1- (point)) (point)))))
          (insert " :")
        ;;add parens if necessary
      (if (and
           (eq needed-punc 'parens)
           (not (string-equal ")" (buffer-substring (1- (point)) (point)))))
          (insert " ()")
        (if (eq needed-punc 'unindent)
            (progn
              (beginning-of-line)
              (delete-char 2)
              (end-of-line)))))
      (newline)
      (occam-indent-line next-line-indent-level)
      (end-of-line))))

;;this function moves the cursor momentarily to the keyword of the current
;;scope
(defun blink-new-keyword ()
  (save-excursion
    ;;now move the cursor to the last keyword
    (let ((keyword nil)(done nil)(current-indent-level)(last-indent-level))
      (setq current-indent-level (current-column))
      (while (and (null keyword) (not done))
        (beginning-of-line 0)
        (back-to-indentation)
        (setq last-indent-level (current-column))
        ;;skip everything which is indented more than the last command
        (if (< last-indent-level current-indent-level)
            (setq keyword (get-key-word-this-line)))
        (if (bobp) (setq done 't)))
      ;;after found keyword, flash it
      (back-to-indentation)
      (if (pos-visible-in-window-p)
          (sit-for 1)
        ;;if we can't see it then do this
                  (message
                   "Matches %s"
                   (if (save-excursion
                         (skip-chars-backward " \t")
                         (not (bolp)))
                       (buffer-substring (progn (beginning-of-line) (point))
                                         (1+ (point)))
                     (buffer-substring (point)
                                       (progn
                                        (forward-char 1)
                                        (skip-chars-forward "\n \t")
                                        (end-of-line)
                                        (point)))))))))

;;this function also blinks to the keyword which the current
;;line is in the scope of (following the delete.
(defun backward-delete-unindent ()
  "Delete and unindent"
  (interactive)
  (let ((p (point)))
    (skip-chars-backward " \t" (- p (current-column)))
    (if (bolp)
        (progn
          (goto-char p)
          (if (bolp)
              (delete-char -1)
            (delete-char (- occam-indent))
            (blink-new-keyword)
            ))
      (progn
        (goto-char p)
        (delete-char -1)))))

(defun occam-keyword (keywords)
  "upcase current word and return 't if it is an OCCAM keyword;
 return nil if it is not"
  (save-excursion
    (let ((eow (point)))
      (forward-word -1)
      (let ((bow (point)))
        (if (re-search-backward "\<" (- bow (current-column)) t)
            nil
          (if (word-in-list (upcase (buffer-substring bow eow))
                            keywords)
              ;;if it is a keyword then
              ;; if we are not in a comment uppercase  
              (if (re-search-backward "--" (- (point) (current-column)) t)
                  't
                (not (upcase-region bow eow)))
            nil))))))

(defun word-in-list (word words)
  "t if word occurs in words, nil otherwise"
  (if (null words)
      nil
    (if (string-equal word (car words))
        t
      (word-in-list word (cdr words)))))

(if (featurep 'hilit19)
  (hilit-set-mode-patterns
   'occam-mode
   '(;; comments
     ("--.*$" nil comment)
     ;; main structure
     ("\\(PROC\\|FUNCTION\\)[^(]*" nil defun)
     ("[!?]" nil label)
     ;("[;,]" nil error)
     ("::" nil jargon-keyword)
     ("\\(\\bCHAN OF\\b\\|\\bPORT OF\\b\\|\\bPLACE\\b\\|\\bVAL\\b\\|\\bBOOL\\b\\|\\bBYTE\\b\\|\\bTIMER\\b\\|\\bINT\\b\\|\\bINT16\\b\\|\\bINT32\\b\\|\\bINT64\\b\\|\\bREAL\\b\\|\\bREAL32\\b\\|\\bREAL64\\b\\)[^:),;
]+[:),;]" nil define)
     ("\\(\\bSEQ\\b\\|\\bPAR\\b\\|\\bIF\\b\\|\\bALT\\b\\|\\bWHILE\\b\\|\\bCASE\\b\\|VALOF\\|PROCESSOR\\|\\bPLACED\\b\\|\\bPRI\\b\\)" nil keyword)
     ("\\(\\bAFTER\\b\\|\\bAND\\b\\|\\bANY\\b\\|\\bAT\\b\\|\\bBITAND\\b\\|\\bBITNOT\\b\\|\\bBITOR\\b\\|\\bFALSE\\b\\|\\bFOR\\b\\|\\bFROM\\b\\|\\bIS\\b\\|\\bMINUS\\b\\|\\bMOSTNEG\\b\\|\\bMOSTPOS\\b\\|\\bNOT\\b\\|\\bOR\\b\\|\\bPLUS\\b\\|\\bPROTOCOL\\b\\|\\bREM\\b\\|\\bRESULT\\b\\|\\bRETYPES\\b\\|\\bROUND\\b\\|\\bSIZE\\b\\|\\bSKIP\\b\\|\\bSTOP\\b\\|\\bTIMES\\b\\|\\bTRUE\\b\\|\\bTRUNC\\b\\)" nil include)
     (":=" nil decl)
     ("^[ ]*\\(#INCLUDE\\|#USE\\|#[ ]*ifdef\\|#[ ]*ifndef\\|#[ ]*elif\\|#[ ]*else\\|#[ ]*endif\\)" nil error)
     ;; The following doesn't work when there are strings in comments?....
     ("\"[^\"]*\"" nil rule)
     ("\\(\\\[\\||\\|\\\]\\)" nil include))))

;; font-lock keywords
(defvar occam-font-lock-keywords
  (list
   '("--.*$" 0 font-lock-comment-face t)
   '("^\\([ \t]\\)*\\(INLINE \\)*\\(PROC\\|FUNCTION\\)\\([^(]*\\)" 
     (2 font-lock-variable-name-face t t)
     (3 font-lock-keyword-face) 
     (4 font-lock-function-name-face t))
   '("[!?]" 0 font-lock-function-name-face nil)
   '("::" 0 font-lock-type-face t)
   '("^[ ]*\\(#INCLUDE\\|#USE\\|#PRAGMA\\|#[ ]*ifdef\\|#[ ]*ifndef\\|#[ ]*elif\\|#[ ]*else\\|#[ ]*endif\\)" 1 font-lock-reference-face t)
   '("\"[^\"]*\"" 0 font-lock-string-face t)
   '(":=" 0 font-lock-keyword-face t)
   '("\\(\\bVAL\\b\\|\\bBOOL\\b\\|\\bBYTE\\b\\|\\bINT\\b\\|\\bINT16\\b\\|\\bINT32\\b\\|\\bINT64\\b\\|\\bREAL\\b\\|\\bREAL32\\b\\|\\bREAL64\\b\\)"
     (0 font-lock-type-face))		; the type info
   '("\\(\\bSEQ\\b\\|\\bPAR\\b\\|\\bIF\\b\\|\\bALT\\b\\|\\bWHILE\\b\\|\\bCASE\\b\\|VALOF\\|PROCESSOR\\|\\bPLACED\\b\\|\\bPRI\\b\\)" 
     0 font-lock-keyword-face nil)
   '("\\(\\bAFTER\\b\\|\\bAND\\b\\|\\bANY\\b\\|\\bAT\\b\\|\\bBITAND\\b\\|\\bBITNOT\\b\\|\\bBITOR\\b\\|\\bFALSE\\b\\|\\bFOR\\b\\|\\bFROM\\b\\|\\bIS\\b\\|\\bMINUS\\b\\|\\bMOSTNEG\\b\\|\\bMOSTPOS\\b\\|\\bNOT\\b\\|\\bOR\\b\\|\\bPLUS\\b\\|\\bPROTOCOL\\b\\|\\bREM\\b\\|\\bRESULT\\b\\|\\bRETYPES\\b\\|\\bROUND\\b\\|\\bSIZE\\b\\|\\bSKIP\\b\\|\\bSTOP\\b\\|\\bTIMES\\b\\|\\bTRUE\\b\\|\\bTRUNC\\b\\)" 
     0 font-lock-reference-face nil)
   '("\\(^[ \t]*\\|([ \t]*\\)\\(VAL \\)*\\(\\[[]a-zA-Z0-9 \t.]+\\]\\)*\\(CHAN OF[ \t]+[^ \t]*\\|PORT OF[ \t]+[^ \t]*\\|\\bPLACE\\b\\|\\bBOOL\\b\\|\\bBYTE\\b\\|\\bTIMER\\b\\|\\bINT\\b\\|\\bINT16\\b\\|\\bINT32\\b\\|\\bINT64\\b\\|\\bREAL\\b\\|\\bREAL32\\b\\|\\bREAL64\\b\\)\\([^:),;
]+\\)[:),;]" 
     (2 font-lock-type-face t t)	; optional `VAL' part
     (3 font-lock-variable-name-face t t)	; optional array part
     (4 font-lock-type-face t)		; the type info
     (5 font-lock-variable-name-face))	; the variable name
  "Expressions to hightlight in Occam mode."))

(setq auto-mode-alist (cons '("\\.occ$" . occam-mode) auto-mode-alist))

(provide 'occam-mode)
