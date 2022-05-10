// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[P1]A.[P2]C, Self.[P1]A : P3, Self.[P1]B : P4, Self.[P1]A.[P2]C == Self.[P1]B.[P2]C>
public protocol P1 {
  associatedtype A: P3 where A.C == Self
  associatedtype B: P4 where B.C == Self
}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self where Self : CaseIterable, Self : Hashable, Self : RawRepresentable, Self.[P2]C : P1, Self.[P2]D : P5, Self.[RawRepresentable]RawValue == String>
public protocol P2: Hashable, RawRepresentable, CaseIterable where RawValue == String {
  associatedtype C: P1
  associatedtype D: P5
}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self : P2>
public protocol P3: P2 {}

// CHECK-LABEL: .P4@
// CHECK-NEXT: Requirement signature: <Self where Self : P2>
public protocol P4: P2 {}

// CHECK-LABEL: .P5@
// CHECK-NEXT: Requirement signature: <Self where Self : CaseIterable, Self : Hashable, Self : RawRepresentable, Self.[RawRepresentable]RawValue == String>
public protocol P5: Hashable, RawRepresentable, CaseIterable where RawValue == String {}
