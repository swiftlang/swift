#!/bin/bash

tmpfile=$(mktemp "/tmp/emacs-compile.XXXXXX")

cat <<EOF > $tmpfile
;; Turn off y-or-no-p
(defalias 'yes-or-no-p 'y-or-n-p)
;; Set the skip threshold to 2 so that we jump only to errors, not warnings.
(setq compilation-skip-threshold 2)

;; Now that we are configured, call compile and set the main window.
(compile "$@")
(switch-to-buffer "*compilation*")
(delete-other-windows)
EOF

emacs -Q -l $tmpfile
