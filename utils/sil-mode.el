;; Create mode-specific tables.
(defvar sil-mode-syntax-table nil
  "Syntax table used while in SIL mode.")

(defvar sil-font-lock-keywords
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
   `(,(regexp-opt '("class" "init" "destructor" "extension" "func"
                    "import" "protocol" "static" "struct" "subscript"
                    "typealias" "enum" "var" "where")
                  'words) . font-lock-keyword-face)
   ;; SIL Types
   '("\\b[$][*]?[A-Z][z-aA-Z_[0-9]*\\b" . font-lock-type-face)

   ;; Statements
   `(,(regexp-opt '("if" "in" "else" "for" "do" "while" "return" "break"
                    "continue" "switch" "case")
                  'words) . font-lock-keyword-face)   
   ;; SIL Stage
   '("sil_stage" . font-lock-keyword-face)
   ;; SIL Function
   `(,(regexp-opt '("sil" "internal" "thunk")
                  'words) . font-lock-keyword-face)
   ;; SIL Declaration
   `(,(regexp-opt '("getter" "setter" "allocator" "initializer" "enumelt"
		    "destroyer" "globalaccessor" "objc") 'words) .
		    font-lock-keyword-face)
   ;; Expressions
   `(,(regexp-opt '("new") 'words) . font-lock-keyword-face)
   ;; SIL Instructions - Allocation/Deallocation.
   `(,(regexp-opt '("alloc_stack" "alloc_ref" "alloc_box" "alloc_array"
		    "dealloc_stack" "dealloc_box" "dealloc_ref")
		  'words) . font-lock-keyword-face)
   ;; SIL Instructions - Accessing Memory.
   `(,(regexp-opt '("load" "store" "assign" "initialize_var" "copy_addr"
		    "destroy_addr" "index_addr" "index_raw_pointer" "to")
		  'words) . font-lock-keyword-face)
   ;; SIL Instructions - Reference Counting.
   `(,(regexp-opt '("strong_retain" "strong_retain_autoreleased" "strong_release"
		    "strong_retain_unowned" "unowned_retain" "unowned_release"
		    "load_weak" "store_weak")
		  'words) . font-lock-keyword-face)
   ;; Literals
   `(,(regexp-opt '("function_ref" "builtin_function_ref" "global_addr"
		    "integer_literal" "float_literal" "string_literal"
		    "builtin_zero" "module") 'words) . font-lock-keyword-face)
   ;; Dynamic Dispatch
   `(,(regexp-opt '("class_method" "super_method" "archetype_method"
		    "protocol_method") 'words) . font-lock-keyword-face)
   ;; Function Application
   `(,(regexp-opt '("apply" "partial_apply" "specialize")
		  'words) . font-lock-keyword-face)
   ;; Metatypes
   `(,(regexp-opt '("metatype" "class_metatype" "archetype_metatype"
		    "protocol_metatype") 'words) . font-lock-keyword-face)
   ;; Aggregate Types
   `(,(regexp-opt '("tuple" "tuple_extract" "tuple_element_addr" "struct"
		    "struct_extract" "struct_element_addr" "ref_element_addr")
		  'words) . font-lock-keyword-face)
   ;; Protocol and Protocol Composition Types
   `(,(regexp-opt '("init_existential" "upcast_existential" "deinit_existential"
		    "project_existential" "init_existential_ref"
		    "upcast_existential_ref" "project_existential_ref")
		  'words) . font-lock-keyword-face)
   ;; Unchecked Conversions
   `(,(regexp-opt '("coerce" "upcast" "archetype_ref_to_super"
		    "address_to_pointer" "pointer_to_address"
		    "ref_to_object_pointer" "object_pointer_to_ref"
		    "ref_to_raw_pointer" "raw_pointer_to_ref"
		    "convert_function" "convert_cc" "bridge_to_block"
		    "thin_to_thick_function") 'words) . font-lock-keyword-face)
   ;; Checked Conversions
   `(,(regexp-opt '("is_nonnull" "downcast" "super_to_archetype_ref"
		    "downcast_archetype_ref" "downcast_archetype_addr"
		    "project_downcast_existential_addr"
		    "downcast_existential_ref")
		  'words) . font-lock-keyword-face)
   ;; Terminators
   `(,(regexp-opt '("unreachable" "return" "autorelease_return" "br"
		    "condbranch" "switch_int" "switch_enum")
		  'words) . font-lock-keyword-face)
   ;; SIL Value
   '("\\b[%][A-Za-z_0-9]+\\([#][0-9]+\\)?\\b" . font-lock-variable-name-face)
   ;; Variables
   '("[a-zA-Z_][a-zA-Z_0-9]*" . font-lock-variable-name-face)
   ;; Unnamed variables
   '("$[0-9]+" . font-lock-variable-name-face)

   )
  "Syntax highlighting for SIL"
  )

;; ---------------------- Syntax table ---------------------------

(if (not sil-mode-syntax-table)
    (progn
      (setq sil-mode-syntax-table (make-syntax-table))
      (mapcar (function (lambda (n)
                          (modify-syntax-entry (aref n 0)
                                               (aref n 1)
                                               sil-mode-syntax-table)))
              '(
                ;; whitespace (` ')
                [?\f  " "]
                [?\t  " "]
                [?\   " "]
                ;; word constituents (`w')
                ;; comments
                [?/  ". 124"]
                [?*  ". 23b"]
                [?\n  ">"]
                [?\^m  ">"]
                ;; symbol constituents (`_')
                [?_ "w"]
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
                ;; character quote ('"')
                [?\' "\""]
                ))))

;; --------------------- Abbrev table -----------------------------

(defvar sil-mode-abbrev-table nil
  "Abbrev table used while in SIL mode.")
(define-abbrev-table 'sil-mode-abbrev-table ())

(defvar sil-mode-hook nil)
(defvar sil-mode-map nil)   ; Create a mode-specific keymap.

(if (not sil-mode-map)
    ()  ; Do not change the keymap if it is already set up.
  (setq sil-mode-map (make-sparse-keymap))
  (define-key sil-mode-map "\t" 'tab-to-tab-stop)
  (define-key sil-mode-map "\es" 'center-line)
  (define-key sil-mode-map "\eS" 'center-paragraph))


(defun sil-mode ()
  "Major mode for editing SIL source files.
  \\{sil-mode-map}
  Runs sil-mode-hook on startup."
  (interactive)
  (kill-all-local-variables)
  (use-local-map sil-mode-map)         ; Provides the local keymap.
  (setq major-mode 'sil-mode)          

  (make-local-variable 'font-lock-defaults)
  (setq major-mode 'sil-mode           ; This is how describe-mode
                                         ;   finds the doc string to print.
  mode-name "Sil"                      ; This name goes into the modeline.
  font-lock-defaults `(sil-font-lock-keywords))

  (setq local-abbrev-table sil-mode-abbrev-table)
  (set-syntax-table sil-mode-syntax-table)
  (setq comment-start "//")
  (run-hooks 'sil-mode-hook))          ; Finally, this permits the user to
                                        ;   customize the mode with a hook.

;; Associate .sil files with sil-mode
(setq auto-mode-alist
   (append '(("\\.sil$" . sil-mode)) auto-mode-alist))

(provide 'sil-mode)
;; end of sil-mode.el
