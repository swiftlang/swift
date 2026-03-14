// RUN: %target-swift-ide-test -repl-code-completion -source-filename %s | %FileCheck %s

// Make sure we partially type check the function body even if function body is
// missing the right brace.
// Don't add any tests at the end of the file!
//
// CHECK-DAG: {{^}}a() -> Void{{$}}
struct FooStruct {
  func a() {}
}
func f() {
  var foo = FooStruct()
  foo.
