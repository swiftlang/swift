protocol P1 {
  func foo()
}
protocol P2 {
  func bar()
}
class C1: P1, P2 {}

// RUN: %sourcekitd-test -req=sema %s -- %s | %FileCheck %s
// CHECK: key.description: "type 'C1' does not conform to protocol 'P1'"
// CHECK: key.description: "type 'C1' does not conform to protocol 'P2'"
