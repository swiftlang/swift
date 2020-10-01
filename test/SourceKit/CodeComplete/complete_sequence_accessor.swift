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
// RUN:   -req=complete -pos=12:9 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=15:15 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=16:15 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=23:9 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=26:15 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=27:15 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=30:9 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=33:15 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=34:15 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=12:1 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=23:1 %s -- %s -parse-as-library == \
// RUN:   -req=complete -pos=16:1 %s -- %s -parse-as-library > %t.response
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response

// globalValImplicit
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// globalValGetSet(get)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// globalValGetSet(set)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// propertyImplicit
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// propertyGetSet(get)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// propertyGetSet(set)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// subscript(implicit getter)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// subscript(get)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// subscript(set)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// accessor top (global var)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "get"
// RESULT-DAG: key.description: "set"
// RESULT-DAG: key.description: "willSet"
// RESULT-DAG: key.description: "didSet"
// RESULT-DAG: key.description: "Foo"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// accessor top (property)
// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "get"
// RESULT-DAG: key.description: "set"
// RESULT-DAG: key.description: "willSet"
// RESULT-DAG: key.description: "didSet"
// RESULT-DAG: key.description: "Foo"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// accessor second (global var)
// RESULT-LABEL: key.results: [
// RESULT-NOT: key.description: "Foo"
// RESULT-DAG: key.description: "get"
// RESULT-DAG: key.description: "set"
// RESULT-DAG: key.description: "willSet"
// RESULT-DAG: key.description: "didSet"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1
