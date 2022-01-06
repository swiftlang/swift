// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

class C {}

struct S {}

// CHECK-LABEL: superclass_and_concrete_requirement.(file).P1@
// CHECK-NEXT: Requirement signature: <Self where Self.T == C>
protocol P1 {
  associatedtype T where T : C, T == C
}

// CHECK-LABEL: superclass_and_concrete_requirement.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self.T == S>
protocol P2 {
  associatedtype T where T : C, T == S
}

// CHECK-LABEL: superclass_and_concrete_requirement.(file).P3@
// CHECK-NEXT: Requirement signature: <Self where Self.T == S>
protocol P3 {
  associatedtype T where T == S, T : C
}
