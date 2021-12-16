// RUN: %target-swift-frontend -typecheck %s -requirement-machine-protocol-signatures=on -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: same_type_requirements_in_protocol.(file).P1@
// CHECK-NEXT: Requirement signature: <Self where Self.X : P3, Self.X == Self.Y.X, Self.Y : P2>
public protocol P1 {
  associatedtype X: P3
  associatedtype Y: P2 where Y.X == X
}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self : P1>
public protocol P2 : P1 {}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).P3@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.Z.X, Self.Z : P2>
public protocol P3 {
  associatedtype Z: P2 where Z.X == Self
}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).Q1@
// CHECK-NEXT: Requirement signature: <Self>
public protocol Q1 {
    associatedtype X
    associatedtype Y
}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).Q2@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.Z.B, Self.Z : Q3>
public protocol Q2 {
    associatedtype Y
    associatedtype Z: Q3 where Z.B == Self
}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).Q3@
// CHECK-NEXT: Requirement signature: <Self where Self.A : Q2, Self.B : Q1, Self.A.Y == Self.B.Y>
public protocol Q3 {
    associatedtype A: Q2
    associatedtype B: Q1 where A.Y == B.Y
}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).R1@
// CHECK-NEXT: Requirement signature: <Self where Self.B : R1>
protocol R1 {
  associatedtype A
  associatedtype B : R1
}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).R2@
// CHECK-NEXT: Requirement signature: <Self>
protocol R2 {
  associatedtype C
}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).R3@
// CHECK-NEXT: Requirement signature: <Self where Self : R1, Self : R2, Self.A == Self.C, Self.B : R3, Self.C == Self.B.A>
protocol R3 : R1, R2 where B : R3, A == B.A, C == A {}


// CHECK-LABEL: same_type_requirements_in_protocol.(file).S1@
// CHECK-NEXT: Requirement signature: <Self>
protocol S1 {}

// CHECK-LABEL: same_type_requirements_in_protocol.(file).S2@
// CHECK-NEXT: Requirement signature: <Self where Self.X : S1, Self.X == Self.Y.X, Self.Y : S2>
protocol S2 {
  associatedtype X : S1
  associatedtype Y: S2 where Y.X == X
}
