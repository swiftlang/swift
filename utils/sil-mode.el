;===--- sil-mode.el --------------------------------------------------------===;
;
; This source file is part of the Swift.org open source project
;
; Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
; Licensed under Apache License v2.0 with Runtime Library Exception
;
; See http://swift.org/LICENSE.txt for license information
; See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
;
;===------------------------------------------------------------------------===;

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
   `(,(regexp-opt '("class" "init" "deinit" "extension" "func"
                    "import" "protocol" "static" "struct" "subscript"
                    "typealias" "enum" "var" "let" "where"  "sil_vtable"
                    "sil_global" "private" "public" "internal" "override"
                    "sil_witness_table")
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
   ;; SIL Linkage
   `(,(regexp-opt '("public" "hidden" "private" "shared" "public_external"
                    "hidden_external" "shared_external" "private_external")
                  'words) . font-lock-keyword-face)
   ;; SIL Declaration
   `(,(regexp-opt '("getter" "setter" "allocator" "initializer" "enumelt"
                    "destroyer" "globalaccessor" "objc") 'words) .
		    font-lock-keyword-face)
   ;; SIL Instructions - Allocation/Deallocation.
   `(,(regexp-opt '("alloc_stack" "alloc_ref" "alloc_ref_dynamic" "alloc_box"
                    "dealloc_stack" "dealloc_box" "dealloc_ref")
		  'words) . font-lock-keyword-face)
   ;; SIL Instructions - Debug Information.
   `(,(regexp-opt '("debug_value" "debug_value_addr")
                  'words) . font-lock-keyword-face)
   ;; SIL Instructions - Accessing Memory.
   `(,(regexp-opt '("load" "store" "assign"  "mark_uninitialized"
                    "mark_function_escape" "copy_addr" "destroy_addr"
                    "index_addr" "index_raw_pointer" "to")
		  'words) . font-lock-keyword-face)
   ;; SIL Instructions - Reference Counting.
   `(,(regexp-opt '("strong_retain" "strong_retain_autoreleased"
                    "strong_release" "strong_retain_unowned" "ref_to_unowned"
                    "unowned_to_ref" "unowned_retain" "unowned_release"
                    "ref_to_unmanaged" "unmanaged_to_ref"
                    "load_weak" "store_weak" "fix_lifetime" "mark_dependence"
                    "strong_unpin" "strong_pin")
		  'words) . font-lock-keyword-face)
   ;; Literals
   `(,(regexp-opt '("function_ref"
                    "integer_literal" "float_literal" "string_literal"
                    "global_addr"
                    ) 'words) . font-lock-keyword-face)
   ;; Dynamic Dispatch
   `(,(regexp-opt '("class_method" "super_method" "witness_method"
                    "dynamic_method")
                  'words) . font-lock-keyword-face)
   ;; Function Application
   `(,(regexp-opt '("apply" "partial_apply" "builtin")
		  'words) . font-lock-keyword-face)
   ;; Metatypes
   `(,(regexp-opt '("metatype" "value_metatype"
                    "existential_metatype" "init_existential_metatype")
                  'words) . font-lock-keyword-face)
   ;; Aggregate Types
   `(,(regexp-opt '("retain_value" "release_value" "tuple" "tuple_extract"
                    "tuple_element_addr" "struct" "struct_extract"
                    "struct_element_addr" "ref_element_addr")
                  'words) . font-lock-keyword-face)
   ;; Enums. *NOTE* We do not include enum itself here since enum is a
   ;; swift declaration as well handled at the top.
   `(,(regexp-opt '("init_enum_data_addr" "unchecked_enum_data"
                    "unchecked_take_enum_data_addr" "inject_enum_addr"
                    "select_enum" "select_value" "select_enum_addr")
                  'words) . font-lock-keyword-face)
   ;; Protocol and Protocol Composition Types
   `(,(regexp-opt '("init_existential_addr" "deinit_existential_addr"
                    "open_existential_addr"
                    "init_existential_ref"
                    "open_existential_ref")
		  'words) . font-lock-keyword-face)
   ;; Unchecked Conversions
   `(,(regexp-opt '("upcast"
                    "address_to_pointer" "pointer_to_address"
                    "unchecked_addr_cast"
                    "unchecked_ref_cast"
                    "ref_to_raw_pointer" "raw_pointer_to_ref"
                    "convert_function"
                    "thick_to_objc_metatype" "objc_to_thick_metatype"
                    "thin_to_thick_function" "is_nonnull"
                    "unchecked_ref_bit_cast" "unchecked_trivial_bit_cast"
                    "objc_existential_metatype_to_object"
                    "bridge_object_to_word" "bridge_object_to_ref"
                    "word_to_bridge_object" "ref_to_bridge_object"
                    )
                  'words) . font-lock-keyword-face)
   ;; Value Buffer
   `(,(regexp-opt '("alloc_value_buffer" "dealloc_value_buffer"
                    "project_value_buffer") 'words) . font-lock-keyword-face)
   ;; Checked Conversions
   `(,(regexp-opt '("unconditional_checked_cast" "unconditional_checked_cast_addr")
		  'words) . font-lock-keyword-face)
   ;; Runtime Failures
   `(,(regexp-opt '("cond_fail")
		  'words) . font-lock-keyword-face)
   ;; Terminators
   `(,(regexp-opt '("unreachable" "return" "autorelease_return" "br"
                    "cond_br" "switch_value" "switch_enum"
                    "switch_enum_addr" "dynamic_method_br"
                    "checked_cast_br")
                  'words) . font-lock-keyword-face)
   ;; Blocks
   `(,(regexp-opt '("project_block_storage" "init_block_storage_header"
                    "copy_block")
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

(unless sil-mode-syntax-table
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

(unless sil-mode-map
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
  mode-name "SIL"                      ; This name goes into the modeline.
  font-lock-defaults `(sil-font-lock-keywords))

  (setq local-abbrev-table sil-mode-abbrev-table)
  (set-syntax-table sil-mode-syntax-table)
  (setq comment-start "//")
  (setq tab-stop-list (number-sequence 2 120 2))
  (setq tab-width 2)
  (run-hooks 'sil-mode-hook))          ; Finally, this permits the user to
                                        ;   customize the mode with a hook.

;; Associate .sil files with sil-mode
(setq auto-mode-alist
   (append '(("\\.sil$" . sil-mode)) auto-mode-alist))

(provide 'sil-mode)
;; end of sil-mode.el
