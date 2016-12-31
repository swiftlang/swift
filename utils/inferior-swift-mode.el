;===--- inferior-swift.el --------------------------------------------------===;
;
; This source file is part of the Swift.org open source project
;
; Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
; Licensed under Apache License v2.0 with Runtime Library Exception
;
; See https://swift.org/LICENSE.txt for license information
; See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
;
;===------------------------------------------------------------------------===;

;;; Load modes that we build off of.
(require 'cl)
(require 'comint)
(require 'swift-mode)

;;; Declare variables.
(defcustom swift-command "swift"
  "Command to invoke `swift'."
  :group 'swift
  :type 'string)
(defcustom swift-args '()
  "Commandline arguments to pass to `swift-command'."
  :group 'swift
  :type 'string)
(defcustom swift-prompt-regexp "^\\(swift\\) "
  "Prompt for `run-swift'."
  :group 'swift
  :type 'regexp)
(defcustom swift-xcrun-command "xcrun"
  "Command used to invoke `xcrun'."
  :group 'swift
  :type 'string)
(defcustom swift-xcrun-sdk "macosx"
  "SDK to lookup from `xcrun'."
  :group 'swift
  :type 'string)

(defvar swift-comint-buffer nil
  "Swift process variable.")

;;; Entry point for creating the inferior-process from a swift mode buffer.
(defun run-swift ()
  "Run an inferior instance of `swift' inside Emacs."
  (interactive)
  (cl-flet ((chomp (str)
                   "Chomp leading and tailing whitespace from STR."
                   (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                                        str)
                     (setq str (replace-match "" t t str)))
                   str))
    (let* ((swift-sdk-path (chomp
                            (shell-command-to-string
                             (format "%s --show-sdk-path -sdk %s"
                                     swift-xcrun-command swift-xcrun-sdk))))
           (buffer (comint-check-proc "inferior-swift")))
      (pop-to-buffer-same-window
       (if (or buffer (not (derived-mode-p 'inferior-swift-mode))
               (comint-check-proc (current-buffer)))
           (get-buffer-create (or buffer "*inferior-swift*"))
         (current-buffer)))
      (unless buffer
        (apply 'make-comint-in-buffer "inferior-swift" buffer
               swift-xcrun-command nil (list swift-command "-sdk" swift-sdk-path))
        (setq swift-comint-buffer (get-buffer "*inferior-swift*"))
        (inferior-swift-mode)))))

(defun inferior-swift-mode--initialize ()
  "Helper function to initialize inferior-swift"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

;;; Define the derived mode.
(define-derived-mode inferior-swift-mode comint-mode "inferior-swift"
  "Minor inferior mode for working with a swift repl."
  nil "inferior-swift"
  (setq comint-prompt-regexp swift-prompt-regexp)
  (setq comint-prompt-read-only t))

;;; Add initialization to the hook.
(add-hook 'inferior-swift-mode-hook 'inferior-swift-mode--initialize)

;;; Extra utility methods for sending swift definitions to repl.
(defun swift-eval-region (start end)
  (interactive "r")
  (let* ((lines (split-string (buffer-substring start end) "\n"))
         (region-string (mapconcat 'identity (remove-if-not (lambda (x) (not (string-equal x ""))) lines) "; ")))
    (comint-send-string swift-comint-buffer region-string))
  (comint-send-string swift-comint-buffer "\n"))

(defun switch-to-swift ()
  (interactive)
  (pop-to-buffer swift-comint-buffer))

;;; Add some things to the keymap for use in inferior lisp mode.
(define-key swift-mode-map "\C-c\C-r" 'swift-eval-region)
(define-key swift-mode-map "\C-c\C-z" 'switch-to-swift)

;; Provide inferior-swift
(provide 'inferior-swift-mode)

;;; end inferior-swift-mode.el
