// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | %FileCheck %s -check-prefix=A

// rdar://14592634
// Make sure we can code complete at EOF inside a function that does not have a
// closing brace.
// Don't add any tests at the end of the file!
//
// A-DAG: Decl[FreeFunction]/CurrModule: f()[#Void#]{{; name=.+$}}
func f() {
  #^A^#
