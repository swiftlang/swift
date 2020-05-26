// BEGIN State1.swift
// Initial state.
class Foo {
  var x: Int
  var y: Int
  func fooMethod() {}
}
func foo(arg: Foo) {
  _ = arg.
}

// BEGIN State2.swift
// Compatible change: implemented 'Foo.fooMethod()', indentation change, added white line.
class Foo {
    var x: Int
    var y: Int
    func fooMethod() {
        print(x + y)
    }
}

func foo(arg: Foo) {
    _ = arg.
}

// BEGIN State3.swift
// Incompatible change: added 'Foo.z'
class Foo {
    var x: Int
    var y: Int
    var z: Int
    func fooMethod() {
        print(x + y)
    }
}

func foo(arg: Foo) {
    _ = arg.
}

// BEGIN DUMMY.swift

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=8:11 -name file.swift -text-input %t/State1.swift -- file.swift == \
// RUN:   -req=complete -pos=11:13 -name file.swift -text-input %t/State2.swift -- file.swift == \
// RUN:   -req=complete -pos=12:13 -name file.swift -text-input %t/State3.swift -- file.swift > %t.response
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.name: "z"
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-NOT: key.name: "z"
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT-DAG: key.name: "z"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1
