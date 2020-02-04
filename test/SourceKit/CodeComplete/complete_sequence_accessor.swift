class Foo {
  var x: Int = 0
  var y: Int = 0
  func fooMethod() {}
}
struct Bar {
  var a: Int = 0
  var b: Int = 0
  func barMethod() {}
}
var globalValImplicit: Foo {
  Bar().
}
var globalValGetSet: Foo {
  get { Foo(). }
  set { Bar(). }
}

enum S {
  var foo: Foo
  var bar: Bar
  var propertyImplicit: Foo {
    foo.
  }
  var propertyGetSet: Foo {
    get { bar. }
    set { foo. }
  }
  subscript(idx: Foo) -> Foo {
    idx.
  }
  subscript(idx: Bar) -> Foo {
    get { idx. }
    set { idx. }
  }
}


// Enabled.
// RUN: %sourcekitd-test \
// RUN:   -req=track-compiles == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=12:9 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=15:15 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=16:15 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=23:9 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=26:15 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=27:15 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=30:9 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=33:15 %s -- %s == \
// RUN:   -req=complete -req-opts=reuseastcontext=1 -pos=34:15 %s -- %s > %t.response
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response
// RUN: %FileCheck --check-prefix=TRACE  %s < %t.response

// globalValImplicit
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// globalValGetSet(get)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// globalValGetSet(set)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// propertyImplicit
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// propertyGetSet(get)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// propertyGetSet(set)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// subscript(implicit getter)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// subscript(get)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// subscript(set)
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
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
// TRACE-LABEL: key.notification: source.notification.compile-did-finish,
// TRACE: key.description: "completion reusing previous ASTContext (benign diagnostic)"
