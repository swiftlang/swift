;===--- .dir-locals.el ---------------------------------------------------===;
;
; This source file is part of the Swift.org open source project
;
; Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
; Licensed under Apache License v2.0 with Runtime Library Exception
;
; See http://swift.org/LICENSE.txt for license information
; See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
;
;===----------------------------------------------------------------------===;
;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (tab-width . 2)
  (fill-column . 80)
  (eval .
        ;; Load the Swift project's settings.  To suppress this action
        ;; you can put "(provide 'swift-project-settings)" in your
        ;; .emacs
        (unless (featurep 'swift-project-settings)
          ;; Make sure the project's own utils directory is in the
          ;; load path, but don't override any one the user might have
          ;; set up.
          (add-to-list
           'load-path
           (concat
            (let ((dlff (dir-locals-find-file default-directory)))
              (if (listp dlff) (car dlff) (file-name-directory dlff)))
            "utils")
           :append)
            ;; Load our project's settings -- indirectly brings in swift-mode
          (require 'swift-project-settings)))
  (c-file-style . "swift")
  )
 (c++-mode
  (whitespace-style . (face lines indentation:space))
  (eval . (whitespace-mode)))
 (objc-mode
  (whitespace-style . (face lines indentation:space))
  (eval . (whitespace-mode)))
 (c-mode
  (whitespace-style . (face lines indentation:space))
  (eval . (whitespace-mode)))
 (swift-mode
  (whitespace-style . (face lines indentation:space))
  (eval . (whitespace-mode))
  (swift-basic-offset . 2)
  (tab-always-indent . t)))

;; Local Variables:
;; eval: (whitespace-mode -1)
;; End:
