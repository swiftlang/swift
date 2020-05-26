class Foo {
  var x: Int
  var y: Int
  func fooMethod() {}
}
struct Bar {
  var a: Int
  var b: Int
}
extension Bar {
  func barMethod() {}
}
func foo(arg: Foo) {
  _ = arg.
}
_ = Bar(a: 12, b: 42)

// Enabled.
// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=14:11 %s -- %s == \
// RUN:   -req=complete -pos=13:15 %s -- %s == \
// RUN:   -req=complete -pos=16:22 %s -- %s > %t.response
// RUN: %FileCheck --check-prefix=RESULT %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "Foo"
// RESULT-DAG: key.name: "Bar"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// RESULT: key.reusingastcontext: 1
