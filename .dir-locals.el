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
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (tab-width . 2)
  (fill-column . 80)
  (eval let* ((x (dir-locals-find-file default-directory))
              (this-directory (if (listp x) (car x)
                                (file-name-directory x))))
        (unless (featurep 'swift-project-settings)
          (add-to-list 'load-path  (concat this-directory "utils") :append)
          (let ((swift-project-directory this-directory))
          (require 'swift-project-settings)))
        (set (make-local-variable 'swift-project-directory) this-directory))
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
  (swift-find-executable-fn . swift-project-executable-find)
  (swift-syntax-check-fn . swift-project-swift-syntax-check)
  (whitespace-style . (face lines indentation:space))
  (eval . (whitespace-mode))
  (swift-basic-offset . 2)
  (tab-always-indent . t)))

;; Local Variables:
;; eval: (whitespace-mode -1)
;; End:
