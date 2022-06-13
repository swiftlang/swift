# How to setup LSP + Emacs as a C++ IDE for Swift

This document describes how to setup a new emacs installation to use LSP and
other modes to create a C++ IDE for working on the compiler code base. It
enables autocompletion, lookup API at point, as well as formatting, renaming,
and syntax highlighting.

## Setting up Package.el for MELPA

Before we do anything, we need to setup package.el so we can grab packages from
[https://melpa.org/](MELPA) and GNU. This can be done by
including the below in your elisp startup file. Make sure it is run before any
other code is run.

```
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)
```

## Download packages

The packages needed as of this document being written (Jun 2022) are:

* use-package
* company
* lsp-mode
* lsp-ui
* helm-lsp
* lsp-treemacs

One can install these by running the command `package-install` inside emacs.

## Configuring LSP

Finally, now we need to configure out installation so everything is setup
correctly. This can be done by including the following in ones .emacs:

```
(use-package company
  :ensure t
  :config
  ;; Enable completion-as-you-type behavior.
  ;; don't add any dely before trying to complete thing being typed
  ;; the call/response to gopls is asynchronous so this should have little
  ;; to no affect on edit latency
  (setq company-idle-delay 0.1)
  ;; start completing after a single character instead of 3
  (setq company-minimum-prefix-length 1)
  ;; align fields in completions
  (setq company-tooltip-align-annotations t)
  )
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (c-mode-common . lsp)
  :custom
  ;; Prevent lsp from inserting header decorators.
  (lsp-clients-clangd-args '("--header-insertion-decorators=0" "--header-insertion=never"))
  :init
  ;; Enable easy local renaming using LSP
  (bind-key "C-x l" 'lsp-rename)
  :config
  ;; The CAPF back-end provides a bridge to the standard
  ;; completion-at-point-functions facility, and thus works with any major mode
  ;; that defines a proper completion function.
  (setq lsp-completion-provider :capf)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package helm-lsp :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
```
