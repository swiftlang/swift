// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -debug-generic-signatures -typecheck %s 2>&1 | %FileCheck %s

// CHECK-LABEL: Requirement signature: <Self where Self == Self.Dual.Dual, Self.Dual : Adjoint>
public protocol Adjoint {
  associatedtype Dual: Adjoint where Self.Dual.Dual == Self
}

// CHECK-LABEL: Requirement signature: <Self>
public protocol Diffable {
  associatedtype Patch
}

// CHECK-LABEL: Requirement signature: <Self where Self : Adjoint, Self : Diffable, Self.Dual : AdjointDiffable, Self.Patch : Adjoint, Self.Dual.Patch == Self.Patch.Dual>
public protocol AdjointDiffable: Adjoint & Diffable
where Self.Patch: Adjoint, Self.Dual: AdjointDiffable,
      Self.Patch.Dual == Self.Dual.Patch {
}

// This is another example used to hit the same problem. Any one of the three
// conformance requirements 'A : P', 'B : P' or 'C : P' can be dropped and
// proven from the other two, but dropping two or more conformance requirements
// leaves us with an invalid signature.
//
// Note that this minimization is still not quite correct; the requirement
// 'Self.B.C == Self.A.A.A' is unnecessary.

// CHECK-LABEL: Requirement signature: <Self where Self.A : P, Self.A == Self.B.C, Self.B : P, Self.B == Self.A.C, Self.C == Self.A.B, Self.B.C == Self.A.A.A>
protocol P {
  associatedtype A : P where A == B.C
  associatedtype B : P where B == A.C
  // expected-note@-1 {{conformance constraint 'Self.C' : 'P' implied here}}
  associatedtype C : P where C == A.B
  // expected-warning@-1 {{redundant conformance constraint 'Self.C' : 'P'}}
}