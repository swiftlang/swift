// RUN: %target-typecheck-verify-swift %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// The GenericSignatureBuilder was unable to derive the redundant conformance requirement 'B: P4'
// in protocol P1.
//
// This version includes the requirement; see interdependent_protocol_conformance_example_2 for
// the other case.

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]A : P2, Self.[P1]B == Self.[P1]A.[P2]B>
protocol P1 {
  associatedtype A: P2 where Self.A.B == Self.B
  associatedtype B: P4
}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]B == Self.[P2]C.[P3]B, Self.[P2]C : P3>
protocol P2 {
  associatedtype B
  associatedtype C: P3 where Self.C.B == Self.B
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3]B : P4>
protocol P3 {
  associatedtype B: P4
}

// CHECK-LABEL: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[P4]A.[P2]B, Self.[P4]A : P2>
protocol P4 {
  associatedtype A: P2 where Self.A.B == Self
}

func takesP4<T : P4>(_: T.Type) {}

func testP1<T : P1>(_: T) {
  takesP4(T.B.self)
}
