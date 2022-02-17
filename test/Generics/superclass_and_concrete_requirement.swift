// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

class C {}

struct S {}

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]T == C>
protocol P1 {
  associatedtype T where T : C, T == C
}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]T == S>
protocol P2 {
  associatedtype T where T : C, T == S
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3]T == S>
protocol P3 {
  associatedtype T where T == S, T : C
}

protocol P4a {
  associatedtype T : C
}

// CHECK-LABEL: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4]T : P4>
protocol P4 {
  associatedtype T : P4 where T.T == S
}
