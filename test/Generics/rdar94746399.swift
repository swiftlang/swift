// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .P1@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[P1]T.[P2]T, Self.[P1]T : P2>
protocol P1 {
  associatedtype T: P2 where T.T == Self
}

// CHECK-LABEL: .P2@
// CHECK-NEXT: Requirement signature: <Self where Self == Self.[P2]T.[P1]T, Self.[P2]T : P1>
protocol P2 {
  associatedtype T: P1 where T.T == Self
}

class SomeClass {}

// CHECK-LABEL: .P3@
// CHECK-NEXT: Requirement signature: <Self where Self : P2, Self.[P2]T : SomeClass>
protocol P3: P2 where T: SomeClass {}

protocol P4 {
  associatedtype T
}

// CHECK-LABEL: .foo@
// CHECK-NEXT: Generic signature: <T where T : P4, T.[P4]T : P1, T.[P4]T.[P1]T : P3>
func foo<T: P4>(_: T) where T.T: P1, T.T.T: P3 {}
