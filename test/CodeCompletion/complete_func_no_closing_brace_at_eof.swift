// Disabled per rdar://14592634
// FIXME: %swift-ide-test -code-completion -source-filename %s -code-completion-token=A | FileCheck %s -check-prefix=A
//
// Run it anyway, to ensure that we at least don't crash.
// RUN: %swift-ide-test -code-completion -source-filename %s -code-completion-token=A

// Make sure we can code complete at EOF inside a function that does not have a
// closing brace.
// Don't add any tests at the end of the file!
//
// A: Begin completions
// A-DAG: SwiftDecl: f()[#Void#]{{$}}
// A: End completions
func f() {
  #^A^#

