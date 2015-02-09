// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | FileCheck %s -check-prefix=A

// Make sure we partially type check the function body even if function body is
// missing the right brace.
// Don't add any tests at the end of the file!
//
// A: Begin completions
// A-DAG: Decl[InstanceMethod]/CurrNominal: a()[#Void#]{{; name=.+$}}
// A: End completions
struct FooStruct {
  func a() {}
}
func f() {
  var foo = FooStruct()
  foo.#^A^#
