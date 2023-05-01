// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | %FileCheck %s -check-prefix=A

// Make sure we can code complete at EOF when there is no newline in the last
// line.
//
// Don't add any tests at the end of the file!
//
// A-DAG: Decl[FreeFunction]/CurrModule: f()[#Void#]{{; name=.+$}}
func f() {}
// There is no newline on the following line.  Don't fix this!
#^A^#