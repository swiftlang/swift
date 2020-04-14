// BEGIN file1.swift
class Foo {
  var x: Int
  var y: Int
}

func foo(arg: Foo) {
  _ = arg.
}

class Bar {
  var a: Int
  var b: Int
  func barMethod() {}
}

// BEGIN file2.swift
extension Foo {
  func fooMethod() {}
}

func bar(arg: Bar) {
  _ = arg.
}

extension Bar {
  func barMethod() {}
}

// BEGIN dummy.swift

// NOTE: Test that switching editing file doesn't trigger fast-completion

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=7:11 %t/file1.swift -- %t/file1.swift %t/file2.swift == \
// RUN:   -req=complete -pos=6:11 %t/file2.swift -- %t/file1.swift %t/file2.swift > %t.response
// RUN: %FileCheck --check-prefix=RESULT  %s < %t.response

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "fooMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "x"
// RESULT-DAG: key.name: "y"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1

// RESULT-LABEL: key.results: [
// RESULT-DAG: key.name: "barMethod()"
// RESULT-DAG: key.name: "self"
// RESULT-DAG: key.name: "a"
// RESULT-DAG: key.name: "b"
// RESULT: ]
// RESULT-NOT: key.reusingastcontext: 1
