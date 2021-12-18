// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

class C {}

// CHECK: superclass_and_layout_requirement.(file).P@
// CHECK: Requirement signature: <Self where Self.T : C>
protocol P {
  associatedtype T : C
}
