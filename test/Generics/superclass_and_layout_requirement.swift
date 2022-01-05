// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

class C {}

// CHECK: superclass_and_layout_requirement.(file).P1@
// CHECK: Requirement signature: <Self where Self.T : C>
protocol P1 {
  associatedtype T : C
}

// CHECK: superclass_and_layout_requirement.(file).P2@
// CHECK: Requirement signature: <Self where Self.T : C>
protocol P2 {
  associatedtype T where T : C, T : AnyObject
}
