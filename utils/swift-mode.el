;; Create mode-specific tables.
(defvar swift-mode-syntax-table nil
  "Syntax table used while in SWIFT mode.")

(defvar swift-font-lock-keywords
  (list
   ;; Comments
   '("//.*" . font-lock-comment-face)
   ;; Strings
   '("\"[^\"]+\"" . font-lock-string-face)
   ;; Types
   '("\\b[A-Z][a-zA-Z_0-9]+\\b" . font-lock-type-face)
   ;; Floating point constants
   '("\\b[-+]?[0-9]+\.[0-9]*\\b" . font-lock-preprocessor-face)
   ;; Integer literals
   '("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face)
   ;; Decl and type keywords
   `(,(regexp-opt '("class" "constructor" "destructor" "extension" "func" "import"
                    "oneof" "protocol" "requires" "struct" "typealias"
                    "var" "static" "subscript") 'words) . font-lock-keyword-face)
   ;; Statements
   `(,(regexp-opt '("if" "in" "else" "for" "do" "while" "return" "break"
                    "continue") 'words) . font-lock-keyword-face)
   ;; Expressions
   `(,(regexp-opt '("new") 'words) . font-lock-keyword-face)
   ;; Variables
   '("[a-zA-Z_][a-zA-Z_0-9]*" . font-lock-variable-name-face)
   ;; Unnamed variables
   '("$[0-9]+" . font-lock-variable-name-face)
   )
  "Syntax highlighting for SWIFT"
  )

;; ---------------------- Syntax table ---------------------------
;; Shamelessly ripped from jasmin.el
;; URL: http://www.neilvandyke.org/jasmin-emacs/jasmin.el.html

(if (not swift-mode-syntax-table)
    (progn
      (setq swift-mode-syntax-table (make-syntax-table))
      (mapcar (function (lambda (n)
                          (modify-syntax-entry (aref n 0)
                                               (aref n 1)
                                               swift-mode-syntax-table)))
              '(
                ;; whitespace (` ')
                [?\^m " "]
                [?\f  " "]
                [?\n  " "]
                [?\t  " "]
                [?\   " "]
                ;; word constituents (`w')
                ;;[?<  "w"]
                ;;[?>  "w"]
                [?\%  "w"]
                ;;[?_  "w  "]
                ;; comments
                [?\;  "< "]
                [?\n  "> "]
                ;;[?\r  "> "]
                ;;[?\^m "> "]
                ;; symbol constituents (`_')
                ;; punctuation (`.')
                ;; open paren (`(')
                [?\( "("]
                [?\[ "("]
                [?\{ "("]
                ;; close paren (`)')
                [?\) ")"]
                [?\] ")"]
                [?\} ")"]
                ;; string quote ('"')
                [?\" "\""]
                ))))

;; --------------------- Abbrev table -----------------------------

(defvar swift-mode-abbrev-table nil
  "Abbrev table used while in SWIFT mode.")
(define-abbrev-table 'swift-mode-abbrev-table ())

(defvar swift-mode-hook nil)
(defvar swift-mode-map nil)   ; Create a mode-specific keymap.

(if (not swift-mode-map)
    ()  ; Do not change the keymap if it is already set up.
  (setq swift-mode-map (make-sparse-keymap))
  (define-key swift-mode-map "\t" 'tab-to-tab-stop)
  (define-key swift-mode-map "\es" 'center-line)
  (define-key swift-mode-map "\eS" 'center-paragraph))


(defun swift-mode ()
  "Major mode for editing SWIFT source files.
  \\{swift-mode-map}
  Runs swift-mode-hook on startup."
  (interactive)
  (kill-all-local-variables)
  (use-local-map swift-mode-map)         ; Provides the local keymap.
  (setq major-mode 'swift-mode)          

  (make-local-variable 'font-lock-defaults)
  (setq major-mode 'swift-mode           ; This is how describe-mode
                                         ;   finds the doc string to print.
  mode-name "Swift"                      ; This name goes into the modeline.
  font-lock-defaults `(swift-font-lock-keywords))

  (setq local-abbrev-table swift-mode-abbrev-table)
  (set-syntax-table swift-mode-syntax-table)
  (setq comment-start "//")
  (run-hooks 'swift-mode-hook))          ; Finally, this permits the user to
                                        ;   customize the mode with a hook.

;; Associate .swift files with swift-mode
(setq auto-mode-alist
   (append '(("\\.swift$" . swift-mode)) auto-mode-alist))

(provide 'swift-mode)
;; end of swift-mode.el
