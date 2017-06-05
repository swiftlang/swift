;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (eval let*
        ((x (dir-locals-find-file default-directory))
         (this-directory (if (listp x) (car x) (file-name-directory x))))
        (unless (or (featurep 'swift-project-settings) 
                    (tramp-tramp-file-p this-directory))
          (add-to-list 'load-path
                       (concat this-directory "utils")
                       :append)
          (let ((swift-project-directory this-directory))
            (require 'swift-project-settings)))
        (set (make-local-variable 'swift-project-directory)
         this-directory)
        )
  (tab-width . 2)
  (fill-column . 80)
  (c-file-style . "swift"))
 (c++-mode
  (whitespace-style face lines indentation:space))
 (c-mode
  (whitespace-style face lines indentation:space))
 (objc-mode
  (whitespace-style face lines indentation:space))
 (prog-mode
  (eval add-hook 'prog-mode-hook
        (lambda nil
          (whitespace-mode 1))
        (not :APPEND)
        :BUFFER-LOCAL))
 (swift-mode
  (swift-find-executable-fn . swift-project-executable-find)
  (swift-syntax-check-fn . swift-project-swift-syntax-check)
  (whitespace-style face lines indentation:space)
  (swift-basic-offset . 2)
  (tab-always-indent . t)))



;; Local Variables:
;; eval: (whitespace-mode -1)
;; End:
