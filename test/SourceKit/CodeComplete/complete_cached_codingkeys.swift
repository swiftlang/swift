struct A {
  var x: Int? {

struct B: Codable {
  static func f() -> X
}

// Make sure the synthesized CodingKeys does not crash by triggering parsing in
// the replaced function body buffer when re-using AST.

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=5:22 %s -- %s == \
// RUN:   -req=complete -pos=5:22 %s -- %s | %FileCheck %s

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.name: "A"
// CHECK-DAG: key.name: "B"
// CHECK-DAG: key.name: "Self"

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.name: "A"
// CHECK-DAG: key.name: "B"
// CHECK-DAG: key.name: "Self"
// CHECK: key.reusingastcontext: 1
