;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (tab-width . 2)
  (c-file-style . "swift")
  (eval .
        (unless (featurep 'swift-mode)
          (load-library
           (concat
            (let ((dlff (dir-locals-find-file default-directory)))
              (if (listp dlff) (car dlff) (file-name-directory dlff)))
            "utils/swift-mode")))))
 (c++mode
  (whitespace-style face lines indentation:space)
  (eval whitespace-mode))
 (swift-mode
  (tab-always-indent t)))

;; Local Variables:
;; eval: (whitespace-mode -1)
;; End:
