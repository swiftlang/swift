// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -debug-generic-signatures -typecheck %s 2>&1 | %FileCheck %s

// https://github.com/apple/swift/issues/56862

// CHECK-LABEL: Requirement signature: <Self where Self == Self.[Adjoint]Dual.[Adjoint]Dual, Self.[Adjoint]Dual : Adjoint>
public protocol Adjoint {
  associatedtype Dual: Adjoint where Self.Dual.Dual == Self
}

// CHECK-LABEL: Requirement signature: <Self>
public protocol Diffable {
  associatedtype Patch
}

// CHECK-LABEL: Requirement signature: <Self where Self : Adjoint, Self : Diffable, Self.[Adjoint]Dual : AdjointDiffable, Self.[Diffable]Patch : Adjoint, Self.[Adjoint]Dual.[Diffable]Patch == Self.[Diffable]Patch.[Adjoint]Dual>
public protocol AdjointDiffable: Adjoint & Diffable
where Self.Patch: Adjoint, Self.Dual: AdjointDiffable,
      Self.Patch.Dual == Self.Dual.Patch {
}

// This is another example used to hit the same problem. Any one of the three
// conformance requirements 'A : P', 'B : P' or 'C : P' can be dropped and
// proven from the other two, but dropping two or more conformance requirements
// leaves us with an invalid signature.

// CHECK-LABEL: Requirement signature: <Self where Self.[P]A : P, Self.[P]A == Self.[P]B.[P]C, Self.[P]B : P, Self.[P]B == Self.[P]A.[P]C, Self.[P]C == Self.[P]A.[P]B>
protocol P {
  associatedtype A : P where A == B.C
  associatedtype B : P where B == A.C
  associatedtype C : P where C == A.B
}
