// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype T
}

protocol P2 {
  associatedtype X : P2 where X.X == X
  associatedtype Y : P1 where Y.T == A
}

struct A : P2 {
  typealias X = A
  struct Y : P1 {
    typealias T = A
  }
}

struct B : P2 {
  typealias X = A
  struct Y : P1 {
    typealias T = A
  }
}

// CHECK-LABEL: concrete_nesting_elimination_order_2.(file).P3a@
// CHECK-NEXT: Requirement signature: <Self where Self : P2, Self.[P2]X == A, Self.[P2]Y == B.Y>

protocol P3a : P2 where X == A,     Y == B.Y {}

// CHECK-LABEL: concrete_nesting_elimination_order_2.(file).P3b@
// CHECK-NEXT: Requirement signature: <Self where Self : P2, Self.[P2]X == A, Self.[P2]Y == B.Y>

protocol P3b : P2 where X == X.Y.T, Y == B.Y {}

// CHECK-LABEL: concrete_nesting_elimination_order_2.(file).P3ab@
// CHECK-NEXT: Requirement signature: <Self where Self : P2, Self.[P2]X == A, Self.[P2]Y == B.Y>

protocol P3ab : P2 where X == A,     X == X.Y.T, Y == B.Y {}

// CHECK-LABEL: concrete_nesting_elimination_order_2.(file).P3ba@
// CHECK-NEXT: Requirement signature: <Self where Self : P2, Self.[P2]X == A, Self.[P2]Y == B.Y>

protocol P3ba : P2 where X == X.Y.T, X == A,     Y == B.Y {}
