class Foo {
  var x: Int
  var y: Int
  func fooMethod() {}
}
struct Bar {
  var a: Int
  var b: Int
  func barMethod() {}
}
func foo(arg: Foo) {
  _ = arg.
}
func bar(arg: Bar) {
  _ = arg.
}

// NOTE: Tests for 'key.codecomplete.reuseastcontex' option.

// Disabled.
// RUN: %sourcekitd-test \
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -pos=12:11 %s -- %s == \
// RUN:   -req=complete -pos=15:11 %s -- %s > %t.response
// RUN: %FileCheck --check-prefix=RESULT %s < %t.response
// RUN: %FileCheck --check-prefix=TRACE_NORMAL %s < %t.response

// Enabled.
// RUN: %sourcekitd-test \
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=12:11 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=15:11 %s -- %s > %t.response.reuseastcontext
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response.reuseastcontext
// RUN: %FileCheck --check-prefix=TRACE_REUSEAST  %s < %t.response.reuseastcontext

// Enabled - compiler argument mismatch.
// RUN: %sourcekitd-test \
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=12:11 %s -- %s -DNOTUSED == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=15:11 %s -- -DNOTUSED %s > %t.response.reuseastcontext_argmismatch
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response.reuseastcontext_argmismatch
// RUN: %FileCheck --check-prefix=TRACE_NORMAL   %s < %t.response.reuseastcontext_argmismatch

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]

// TRACE_NORMAL-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE_NORMAL-NOT: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE_NORMAL-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE_NORMAL-NOT: key.description: "completion reusing previous ASTContext (benign diagnostic)"

// TRACE_REUSEAST-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE_REUSEAST-NOT: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE_REUSEAST-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE_REUSEAST: key.description: "completion reusing previous ASTContext (benign diagnostic)"
