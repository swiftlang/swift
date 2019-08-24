" Vim syntax file
" Language: gyb on swift

runtime! syntax/swift.vim
unlet b:current_syntax

syn include @Python syntax/python.vim
syn region pythonCode matchgroup=gybPythonCode start=+^ *%+ end=+$+ contains=@Python keepend
syn region pythonCode matchgroup=gybPythonCode start=+%{+ end=+}%+ contains=@Python keepend
syn match gybPythonCode /\${[^}]*}/
hi def link gybPythonCode CursorLineNr

let b:current_syntax = "swiftgyb"

