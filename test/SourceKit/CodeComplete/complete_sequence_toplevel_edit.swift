// BEGIN State1.swift
struct Foo {
  var x: Int
  var y: Int
  func fooMethod() {}
}

let globalFoo = Foo(

// BEGIN State2.swift
struct Foo {
    var x: Int
    var y: Int
    func fooMethod() {}
}

let globalFoo = Foo(x: 12, y: 42)

var globalX = globalFoo.

// BEGIN State3.swift
struct Foo {
    var x: Int
    var y: Int
    var z: Int
    func fooMethod() {}
}

let globalFoo = Foo(x: 12, y: 42, z: 68)

guard let val = globalFoo.

func dummy(x: Int) {
  print()
}

// BEGIN State4.swift
struct Foo {
    var x: Int
    var y: Int
    var z: Int
    func fooMethod() {}
}

let globalFoo = Foo(x: 12, y: 42, z: 68)

func dummy(x: ) {
  print()
}

// BEGIN DUMMY.swift

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=7:21 -name file.swift -text-input %t/State1.swift -- file.swift == \
// RUN:   -req=complete -pos=9:25 -name file.swift -text-input %t/State2.swift -- file.swift == \
// RUN:   -req=complete -pos=10:27 -name file.swift -text-input %t/State3.swift -- file.swift == \
// RUN:   -req=complete -pos=10:15 -name file.swift -text-input %t/State4.swift -- file.swift > %t.response
// RUN: %FileCheck --check-prefix=RESULT %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.description: "fooMethod()"
// RESULT-NOT: key.description: "x"
// RESULT-NOT: key.description: "y"
// RESULT-DAG: key.description: "(x: Int, y: Int)",
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.description: "z"
// RESULT-DAG: key.description: "fooMethod()"
// RESULT-DAG: key.description: "self"
// RESULT-DAG: key.description: "x"
// RESULT-DAG: key.description: "y"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.description: "fooMethod()"
// RESULT-DAG: key.description: "self"
// RESULT-DAG: key.description: "x"
// RESULT-DAG: key.description: "y"
// RESULT-DAG: key.description: "z"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.description: "globalFoo"
// RESULT-DAG: key.description: "Foo"
// RESULT: ]
// RESULT: key.reusingastcontext: 1
