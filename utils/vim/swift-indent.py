# This file is a minimal swift-indent vim-integration.  To install:
# - Change 'binary' if swift-indent is not on the path (see below).
# - Add to your .vimrc:
#
#   map <C-I> :pyf <path-to-this-file>/swift-indent.py<cr>
#   imap <C-I> <c-o>:pyf <path-to-this-file>/swift-indent.py<cr>
#
# The first line enables swift-indent for NORMAL and VISUAL mode, the second
# line adds support for INSERT mode.  Change "C-I" to another binding if you
# need swift-indent on a different key (C-I stands for Ctrl+i).
#
# With this integration you can press the bound key and swift-indent will
# indent the current line in NORMAL and INSERT mode or the selected region in
# VISUAL mode.  The line or region is extended to the next bigger syntactic
# entity.
#
# You can also pass in the variable "l:lines" to choose the range for
# indenting.  This variable can either contain "<start line>:<end line> or
# "all" to indent the full file.  So, to indent the full file, write a function
# like:
#
# :function IndentFile()
# :  let l:lines="all"
# :  pyf <path-to-this-file>/swift-indent.py
# :endfunction
#
# It operates on the current, potentially unsaved buffer and does not create or
# save any files.  To revert a indenting, just undo.

from __future__ import print_function

import difflib
import platform
import subprocess
import sys

import vim

binary = 'swift-indent'
if vim.eval('exists("g:swift_indent_path")') == "1":
    binary = vim.eval('g:swift_indent_path')


def get_buffer(encoding):
    if platform.python_version_tuple()[0] == "3":
        return vim.current.buffer
    return [line.decode(encoding) for line in vim.current.buffer]


def main(argc, argv):
    encoding = vim.eval("&encoding")
    buf = get_buffer(encoding)

    if vim.eval('exists("l:lines")') == '1':
        lines = vim.eval('l:lines')
    else:
        lines = '%s:%s' % (vim.current.range.start + 1,
                           vim.current.range.end + 1)

    cursor = int(vim.eval('line2byte(line(".")) + col(".")')) - 2
    if cursor < 0:
        print("Couldn't determine cursor position.  Is your file empty?")
        return

    # avoid the cmd prompt on windows
    SI = None
    if sys.platform.startswith('win32'):
        SI = subprocess.STARTUPINFO()
        SI.dwFlags |= subprocess.STARTF_USESHOWWINDOW
        SI.wShowWindow = subprocess.SW_HIDE

    command = [binary]
    if lines != 'all':
        command.extend(['-line-range', lines])

    p = subprocess.Popen(command,
                         stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                         stdin=subprocess.PIPE, startupinfo=SI)
    stdout, stderr = p.communicate(input='\n'.join(buf).encode(encoding))

    if stderr:
        print(stderr)

    if not stdout:
        print('No output from swift-indent (crashed?).')
        return

    lines = stdout.decode(encoding).split('\n')
    sequence = difflib.SequenceMatcher(None, buf, lines)
    for op in reversed(sequence.get_opcodes()):
        if op[0] != 'equal':
            vim.current.buffer[op[1]:op[2]] = lines[op[3]:op[4]]


if __name__ == '__main__':
    main(len(sys.argv), sys.argv)
