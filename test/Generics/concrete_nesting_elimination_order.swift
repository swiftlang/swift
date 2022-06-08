// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T
}

protocol P2 {}

protocol P3 {
  associatedtype T : P2, P3
}

protocol P4 : P3 {}

protocol P5 : P4 {}

struct C : P2, P5 {
  typealias T = C
}

struct G<T: P5> : P1 {}

// CHECK-LABEL: concrete_nesting_elimination_order.(file).P6@
// CHECK-NEXT: Requirement signature: <Self where Self.[P6]X : P1, Self.[P6]Y : P5, Self.[P6]Y == Self.[P6]Z.[P1]T, Self.[P6]Z : P1>

protocol P6 {
  associatedtype X : P1
  associatedtype Y : P5
  associatedtype Z : P1 where Z.T == Y
}

// CHECK-LABEL: concrete_nesting_elimination_order.(file).P7@
// CHECK-NEXT: Requirement signature: <Self where Self : P6, Self.[P6]X == G<Self.[P6]Y>, Self.[P6]Z == G<Self.[P6]Y>>

protocol P7 : P6 where X == G<Y>, X == Z {}

// CHECK-LABEL: concrete_nesting_elimination_order.(file).P8a@
// CHECK-NEXT: Requirement signature: <Self where Self : P7, Self.[P6]Y == C>

protocol P8a : P7 where Y == C {}

// CHECK-LABEL: concrete_nesting_elimination_order.(file).P8b@
// CHECK-NEXT: Requirement signature: <Self where Self : P7, Self.[P6]Y == C>

protocol P8b : P7 where Y == C {}

// Make sure we pick 'Y == C' and not 'Y == G<C>' here.
