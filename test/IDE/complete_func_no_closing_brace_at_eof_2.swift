// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=A | %FileCheck %s -check-prefix=A

// rdar://14585132
// Make sure we can resolve function parameters if function body is missing the
// right brace.
// Don't add any tests at the end of the file!
//
// A-DAG: Decl[InstanceMethod]/CurrNominal: a()[#Void#]{{; name=.+$}}
struct FooStruct {
  func a() {}
}
func f(foo: FooStruct) {
  foo.#^A^#
