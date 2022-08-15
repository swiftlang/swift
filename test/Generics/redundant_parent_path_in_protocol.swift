// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: redundant_parent_path_in_protocol.(file).P1@
// CHECK-NEXT: Requirement signature: <Self>

protocol P1 {}

// CHECK-LABEL: redundant_parent_path_in_protocol.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]A : P1, Self.[P2]B : P2>

protocol P2 {
  associatedtype A: P1
  associatedtype B: P2
}

struct Concrete: P1, P2 {
  typealias A = Concrete
  typealias B = Concrete
}

// CHECK-LABEL: redundant_parent_path_in_protocol.(file).P3a@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3a]T : P2>

protocol P3a {
  associatedtype T : P2
}

// CHECK-LABEL: redundant_parent_path_in_protocol.(file).P3b@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3b]T : P2, Self.[P3b]T.[P2]A == Self.[P3b]T.[P2]B>

protocol P3b {
  associatedtype T : P2 where T.A == T.B
}

// CHECK-LABEL: redundant_parent_path_in_protocol.(file).P4a@
// CHECK-NEXT: Requirement signature: <Self where Self : P3a, Self.[P3a]T == Concrete>

protocol P4a : P3a where T.A == T.B, T == Concrete {}

// CHECK-LABEL: redundant_parent_path_in_protocol.(file).P4b@
// CHECK-NEXT: Requirement signature: <Self where Self : P3b, Self.[P3b]T == Concrete>

protocol P4b : P3b where T == Concrete {}

// CHECK-LABEL: redundant_parent_path_in_protocol.(file).P5@
// CHECK-NEXT: Requirement signature: <Self where Self.[P5]T == Concrete>

protocol P5 {
  associatedtype T : P2 where T.A == T.B, T == Concrete
}
