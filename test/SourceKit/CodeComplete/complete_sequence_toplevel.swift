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
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -pos=14:11 %s -- %s == \
// RUN:   -req=complete -pos=13:15 %s -- %s == \
// RUN:   -req=complete -pos=16:22 %s -- %s > %t.response
// RUN: %FileCheck --check-prefix=RESULT %s < %t.response
// RUN: %FileCheck --check-prefix=TRACE %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "Foo"
// RESULT-DAG: key.name: "Bar"
// RESULT: ]
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]

// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE-NOT: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
