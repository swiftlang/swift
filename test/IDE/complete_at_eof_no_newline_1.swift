// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | FileCheck %s -check-prefix=A

// Make sure we can code complete at EOF when there is no newline in the last
// line.
//
// Don't add any tests at the end of the file!
//
// A: Begin completions
// A-DAG: Decl[InstanceMethod]/CurrNominal: f()[#Void#]{{$}}
// A: End completions
struct FooStruct {
  func f() {}
}
var fooObject: FooStruct
// There is no newline on the following line.  Don't fix this!
fooObject.#^A^#
