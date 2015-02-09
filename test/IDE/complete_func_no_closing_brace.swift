// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | FileCheck %s -check-prefix=A

// Make sure we don't crash on a function without a closing brace before EOF.
// Don't add any tests at the end of the file!
//
// A: Begin completions
// A-DAG: Decl[FreeFunction]/CurrModule: f()[#Void#]{{; name=.+$}}
// A: End completions
func f() {
  #^A^#
  1 2 3
