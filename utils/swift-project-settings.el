;===--- swift-project-settings.el - Swift project's format conventions ---===;
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
;
;  Emacs-lisp support for automatically formatting things according to this
;  project's conventions.  To prevent this file from being automatically 
;  loaded, add (provide 'swift-project-settings) to your .emacs
;
;===----------------------------------------------------------------------===;


;; Associate .swift files with swift-mode
(setq auto-mode-alist
   (append '(("\\.swift$" . swift-mode) ("\\.gyb$" python-mode t)) auto-mode-alist))

;; Make sure we know where to find swift-mode
(autoload 'swift-mode "swift-mode"
  "Major mode for editing SWIFT source files.
  \\{swift-mode-map}
  Runs swift-mode-hook on startup."
  :interactive
  )

(require 'cc-styles)

;; This style is appropriate for indenting Swift's C++ source
(c-add-style "swift"
             '((c-basic-offset . 2)
               (c-offsets-alist
                (namespace-open . 0)
                (template-args-cont c-lineup-template-args +)
                (knr-argdecl-intro . +)
                (substatement-open . +)
                (substatement-label . 2)
                (label . 2)
                (inline-open . +)
                (inexpr-class . +)
                (defun-open . 0)
                (func-decl-cont . +)
                (knr-argdecl . 0)
                (topmost-intro-cont . c-lineup-topmost-intro-cont)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (block-open . 0)
                (brace-list-open . 0)
                (brace-entry-open . 0)
                (statement-case-open . 0)
                (do-while-closure . 0)
                (catch-clause . 0)
                (stream-op . c-lineup-streamop)
                (cpp-macro-cont . +)
                (friend . 0)
                (objc-method-intro .
                                   [0])
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (extern-lang-open . 0)
                (module-open . 0)
                (composition-open . 0)
                (extern-lang-close . 0)
                (module-close . 0)
                (composition-close . 0)
                (inextern-lang . +)
                (inmodule . +)
                (incomposition . +)
                (inlambda . c-lineup-inexpr-block)
                (lambda-intro-cont . +)
                (inexpr-statement . +)
                (topmost-intro . 0)
                (defun-block-intro . +)
                (statement . 0)
                (defun-close . 0)
                (statement-cont . +)
                (brace-list-intro . +)
                (brace-list-entry . 0)
                (brace-list-close . 0)
                (statement-block-intro . +)
                (block-close . 0)
                (innamespace . 0)
                (inher-intro . +)
                (class-open . 0)
                (inclass . +)
                (access-label . 0)
                (member-init-intro . +)
                (substatement . +)
                (inline-close . 0)
                (class-close . 0)
                (namespace-close . 0)
                (case-label . -)
                (statement-case-intro . +)
                (cpp-define-intro . +)
                (else-clause . 0)
                (arglist-intro . +)
                (arglist-cont . +)
                (c . c-lineup-C-comments)
                (inher-cont . c-lineup-multi-inher)
                (string . -1000)
                (comment-intro . c-lineup-comment)
                (arglist-cont-nonempty . c-lineup-arglist)
                (arglist-close . c-lineup-close-paren)
                (cpp-macro . -1000))))

;; When this file is loaded in response to visiting a file in the
;; project, it won't have had its major mode set up according to the
;; project settings yet.  For example, Swift files may come up in
;; Fundamental mode, and C++ files won't use the swift style, unless
;; we do something.  This hack causes the file to be re-mode-ed.
(set-auto-mode)


(define-skeleton llvm-header
  "Insert the LLVM header at the top of a file

Note: this skeleton presently assumes that comment-start creates
a comment until end-of-line.  Handling paired comment syntax is
possible, but more work, and someone needs to decide what such an
LLVM header should look like.
"
  ;; prompt
  "Short description (RET for none): "

  ;; v1 is comment-start without trailing whitespace.  Presumably
  ;; nobody is crazy enough to define a language where whitespace
  ;; determines whether something is a comment, but c++ mode and
  ;; friends have a space at the end of comment-start, which messes up
  ;; the LLVM header format.
  ;;
  ;; When there's no comment syntax defined, we use "//"; precedent is
  ;; in the project's README file.
  '(setq v1 (replace-regexp-in-string " +\\'" "" (or comment-start "//")))

  ;; v2 is the name of the file
  '(setq v2 (file-name-nondirectory (buffer-file-name)))
  v1 "===--- " v2
  ;; if the short description is empty, eat up the preceding " - "
  " - " str | -3
  " "
  ;; v3 is t if there was a short description
  '(setq v3 (> (length str) 0))
  
  ;; Generate dashes to fill out the rest of the top line
  (make-string (max (- 65 (+ (if v3 (+ 3 (length str)) 0) (length v2))) 3) ?-)
  "===" v1 "\n"

  ;; Use whatever comment character is usual for the current mode in place of "//"
  (replace-regexp-in-string "//" v1 
"//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
")
  ;; if there was a short description, add a section for a longer
  ;; description and leave the cursor there
  (when v3
    (replace-regexp-in-string "//" v1 
"//
//  "))
  (when v3 '_)
  (when v3
    (replace-regexp-in-string "//" v1 "
//
//===----------------------------------------------------------------------===//
")
    ))

(define-skeleton llvm-divider
  "Insert an llvm //===--- ... ---===// divider
"
  ;; prompt
  "Text (RET for none): "

  ;; v1 is comment-start without trailing whitespace.  Presumably
  ;; nobody is crazy enough to define a language where whitespace
  ;; determines whether something is a comment, but c++ mode and
  ;; friends have a space at the end of comment-start, which messes up
  ;; the LLVM header format.
  ;;
  ;; When there's no comment syntax defined, we use "//"; precedent is
  ;; in the project's README file.
  '(setq v1 (replace-regexp-in-string " +\\'" "" (or comment-start "//")))
  
  ;; v2 is either comment-end stripped of leading whitespace, or if it
  ;; is non-empty, v1 all over again
  '(setq v2
         (replace-regexp-in-string "\\` +" ""
          (if (and comment-end (> (length comment-end) 0)) comment-end  v1)))
  
  v1 "===--- "
  str & " " | -1
  '(setq v3 (length str))
  
  ;; Generate dashes to fill out the rest of the top line
  (make-string
   (max 3
        (- 77 (current-column) (length v2)))
   ?-)
  
  "===" v2
  )

(defvar swift-project-auto-insert-alist
  ;; Currently we match any file and insert the LLVM header.  We can
  ;; make the regexp more specific or filter based on mode if this
  ;; doesn't work out.
  '((("" . "LLVM header") . llvm-header))
  "auto-insert-alist entries that are just for the Swift project"
  )

(defadvice auto-insert (around swift-project-auto-insert activate)
  "Modify auto-insert so that swift-project-auto-insert-alist
takes precedence for files in the Swift project"
  ;; Assume that files with c-file-style set to "swift" are
  ;; part of the Swift project.  Because it's set in
  ;; .dir-locals.el, this will apply to all files, not just
  ;; those using cc-mode
  (if (and (boundp 'c-file-style) (equal c-file-style "swift"))
      (let ((auto-insert-alist
             (append swift-project-auto-insert-alist auto-insert-alist))
            ;; The default is to ask when creating a new file.  Inside
            ;; this project, we always want the LLVM header, so only
            ;; prompt if the user has set auto-insert to /always/
            ;; prompt.
            (auto-insert-query (if (eq auto-insert-query 'function) nil auto-insert-query)))
        ad-do-it)
    ad-do-it))

(push 'swift-stdlibunittest1 compilation-error-regexp-alist)
(push `(swift-stdlibunittest1 "^\\(?:out\\|err\\)>>>.* \\(?:failed\\(?: at\\|.*file\\)\\|.*: file\\) \\(.*[^,]\\), line \\([0-9]+\\)$"
              1 2 ,(not :column) ,(not :just-a-warning))
      compilation-error-regexp-alist-alist)

(push 'swift-stdlibunittest2 compilation-error-regexp-alist)
(push `(swift-stdlibunittest2 "^\\(?:out\\|err\\)>>> *#[0-9]+: \\(.+\\):\\([0-9]+\\)\\(?: +.*\\)?$"
              1 2 ,(not :column) ,(not :just-a-warning))
      compilation-error-regexp-alist-alist)
    
(provide 'swift-project-settings)
;; end of swift-project-settings.el
