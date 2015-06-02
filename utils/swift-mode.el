;===--- swift-mode.el ----------------------------------------------------===;
;
; This source file is part of the Swift.org open source project
;
; Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
; Licensed under Apache License v2.0 with Runtime Library Exception
;
; See http://swift.org/LICENSE.txt for license information
; See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
;
;===----------------------------------------------------------------------===;

(require 'compile)
(unless (fboundp 'prog-mode)
  (define-derived-mode prog-mode fundamental-mode "Prog"
    "Base mode for other programming language modes"
    (setq bidi-paragraph-direction 'left-to-right)
    (set
     (make-local-variable 'require-final-newline) mode-require-final-newline)
    (set
     (make-local-variable 'parse-sexp-ignore-comments) t)))

;; Create mode-specific tables.
(defvar swift-mode-syntax-table nil
  "Syntax table used while in SWIFT mode.")

(defvar swift-font-lock-keywords
  (list
   ;; Comments
   '("^#!.*" . font-lock-comment-face)
   ;; Types
   '("\\b[A-Z][a-zA-Z_0-9]*\\b" . font-lock-type-face)
   ;; Floating point constants
   '("\\b[-+]?[0-9]+\.[0-9]+\\b" . font-lock-preprocessor-face)
   ;; Integer literals
   '("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face)
   ;; Decl and type keywords
   `(,(regexp-opt '("class" "init" "deinit" "extension" "func"
                    "import" "let" "protocol" "static" "struct" "subscript"
                    "typealias" "enum" "var" "where"
                    "private" "public" "internal" "override" "throws")
                  'words) . font-lock-keyword-face)
   ;; Statements
   `(,(regexp-opt '("if" "guard" "in" "else" "for" "do" "repeat" "while" "return"
                    "break" "continue" "switch" "case" "throw" "try" "catch")
                  'words) . font-lock-keyword-face)
   ;; Expressions
   `(,(regexp-opt '("new") 'words) . font-lock-keyword-face)
   ;; Special Variables
   '("self" . font-lock-keyword-face)
   ;; Variables
   '("[a-zA-Z_][a-zA-Z_0-9]*" . font-lock-variable-name-face)
   ;; Unnamed variables
   '("$[0-9]+" . font-lock-variable-name-face)
   )
  "Syntax highlighting for SWIFT"
  )

;; ---------------------- Syntax table ---------------------------

(if (not swift-mode-syntax-table)
    (progn
      (setq swift-mode-syntax-table (make-syntax-table))
      (mapcar (function (lambda (n)
                          (modify-syntax-entry (aref n 0)
                                               (aref n 1)
                                               swift-mode-syntax-table)))
              '(
                ;; whitespace (` ')
                [?\f  " "]
                [?\t  " "]
                [?\   " "]
                ;; word constituents (`w')
                ;; punctuation
                [?< "."]
                [?> "."]
                ;; comments
                [?/  ". 124"]
                [?*  ". 23b"]
                [?\n  ">"]
                [?\^m  ">"]
                ;; symbol constituents (`_')
                [?_ "_"]
                ;; punctuation (`.')
                ;; open paren (`(')
                [?\( "())"]
                [?\[ "(]"]
                [?\{ "(}"]
                ;; close paren (`)')
                [?\) ")("]
                [?\] ")["]
                [?\} "){"]
                ;; string quote ('"')
                [?\" "\""]
                ;; escape-syntax characters ('\\')
                [?\\ "\\"]
                ))))

;; --------------------- Abbrev table -----------------------------

(defvar swift-mode-abbrev-table nil
  "Abbrev table used while in SWIFT mode.")
(define-abbrev-table 'swift-mode-abbrev-table ())

(defvar swift-mode-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for `swift-mode'.")


;;;###autoload
(define-derived-mode swift-mode prog-mode "Swift"
  "Major mode for editing SWIFT source files.
  \\{swift-mode-map}
  Runs swift-mode-hook on startup."
  :group 'swift

  (require 'electric)
  (set (make-local-variable 'indent-line-function) 'swift-indent-line)
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'comment-start) "// ")
  (set (make-local-variable 'comment-end) "")

  (unless (boundp 'electric-indent-chars)
    (defvar electric-indent-chars nil))
  (unless (boundp 'electric-pair-pairs)
    (defvar electric-pair-pairs nil))
    
  (set (make-local-variable 'electric-indent-chars)
       (append "{}()[]:," electric-indent-chars))
  (set (make-local-variable 'electric-pair-pairs)
       (append '(
                 ;; (?' . ?\') ;; This isn't such a great idea because
                 ;; pairs are detected even in strings and comments,
                 ;; and sometimes an apostrophe is just an apostrophe
                 (?{ . ?})  (?[ . ?]) (?( . ?)) (?` . ?`)) electric-pair-pairs))
  (set (make-local-variable 'electric-layout-rules)
       '((?\{ . after) (?\} . before)))
  
  (set (make-local-variable 'font-lock-defaults) 
       '(swift-font-lock-keywords) ))

(defun swift-indent-line ()
  (interactive)
  (let (indent-level target-column)
    (save-excursion
      (widen)
      (setq indent-level (car (syntax-ppss (point-at-bol))))
      
      ;; Look at the first non-whitespace to see if it's a close paren
      (beginning-of-line)
      (skip-syntax-forward " ")
      (setq target-column
            (if (equal (char-after) ?\#) 0
              (* 2
                 (- indent-level
                    (cond ((= (char-syntax (or (char-after) ?\X)) ?\))
                           1)
                          ((save-match-data
                             (looking-at
                              "case \\|default *:\\|[a-zA-Z_][a-zA-Z0-9_]*\\(\\s-\\|\n\\)*:\\(\\s-\\|\n\\)*\\(for\\|do\\|\\while\\|switch\\)\\>"))
                           1)
                          (t 0))))))
      (indent-line-to target-column))
    (when (< (current-column) target-column)
      (move-to-column target-column)))
  )


;; Compilation error parsing
(push 'swift0 compilation-error-regexp-alist)
(push 'swift1 compilation-error-regexp-alist)
(push 'swift-fatal compilation-error-regexp-alist)

(push `(swift0
        ,(concat
     "^"
       "[ \t]+" "\\(?:(@\\)?"
       "[A-Z][A-Za-z0-9_]*@" 
     ;; Filename \1
       "\\("
          "[0-9]*[^0-9\n]" 
          "\\(?:" 
             "[^\n :]" "\\|" " [^/\n]" "\\|" ":[^ \n]" 
          "\\)*?" 
       "\\)"
       ":"
       ;; Line number (\2)
       "\\([0-9]+\\)"
       ":"
       ;; Column \3
       "\\([0-9]+\\)"
       )
     1 2 3 0)
      compilation-error-regexp-alist-alist)

(push `(swift1
        ,(concat
     "^"
       "[0-9]+[.][ \t]+While .* at \\[?"
     ;; Filename \1
       "\\("
          "[0-9]*[^0-9\n]" 
          "\\(?:" 
             "[^\n :]" "\\|" " [^/\n]" "\\|" ":[^ \n]" 
          "\\)*?" 
       "\\)"
       ":"
       ;; Line number (\2)
       "\\([0-9]+\\)"
       ":"
       ;; Column \3
       "\\([0-9]+\\)"
       )
     1 2 3 2)
      compilation-error-regexp-alist-alist)

(push `(swift-fatal
        ,(concat
     "^\\(?:assertion failed\\|fatal error\\): \\(?:.*: \\)?file "
     ;; Filename \1
       "\\("
          "[0-9]*[^0-9\n]" 
          "\\(?:" 
             "[^\n :]" "\\|" " [^/\n]" "\\|" ":[^ \n]" 
          "\\)*?" 
       "\\)"
       ", line "
       ;; Line number (\2)
       "\\([0-9]+\\)"
       )
     1 2 nil 2)
      compilation-error-regexp-alist-alist)

(defgroup swift nil
  "Major mode for editing swift source files."
  :prefix "swift-")

(provide 'swift-mode)

;; end of swift-mode.el
