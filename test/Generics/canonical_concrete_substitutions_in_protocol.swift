// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

struct G<T> {}

// CHECK-LABEL: canonical_concrete_substitutions_in_protocol.(file).P@
// CHECK-NEXT: Requirement signature: <Self where Self.[P]A == G<Self.[P]B>, Self.[P]B == Self.[P]T.[Q]X, Self.[P]T : Q>

protocol P {
  associatedtype A where A == G<T.X>
  associatedtype B where B == T.X
  associatedtype T : Q
}

protocol Q {
  associatedtype X
}

protocol QQ : Q {}

protocol R {
  associatedtype A
  associatedtype C: QQ where C.X == G<A>
}

// Make sure substitutions which are themselves concrete simplify recursively.

// CHECK-LABEL: canonical_concrete_substitutions_in_protocol.(file).P1@
// CHECK-NEXT: Requirement signature: <Self where Self.[P1]T == Int, Self.[P1]U == G<Int>>

protocol P1 {
  associatedtype T where T == Int
  associatedtype U where U == G<T>
}

// CHECK-LABEL: canonical_concrete_substitutions_in_protocol.(file).P2@
// CHECK-NEXT: Requirement signature: <Self where Self.[P2]T == Int, Self.[P2]U == G<Int>>

protocol P2 {
  associatedtype U where U == G<T>
  associatedtype T where T == Int
}

// CHECK-LABEL: canonical_concrete_substitutions_in_protocol.(file).P3@
// CHECK-NEXT: Requirement signature: <Self where Self.[P3]T == G<Int>, Self.[P3]U == Int>

protocol P3 {
  associatedtype T where T == G<U>
  associatedtype U where U == Int
}

// CHECK-LABEL: canonical_concrete_substitutions_in_protocol.(file).P4@
// CHECK-NEXT: Requirement signature: <Self where Self.[P4]T == G<Int>, Self.[P4]U == Int>

protocol P4 {
  associatedtype U where U == Int
  associatedtype T where T == G<U>
}

protocol P5a {
  associatedtype T where T == G<U>
  associatedtype U
}

// CHECK-LABEL: canonical_concrete_substitutions_in_protocol.(file).P5@
// CHECK-NEXT: Requirement signature: <Self where Self.[P5]T == G<G<Int>>, Self.[P5]U : P5a, Self.[P5]U.[P5a]U == Int>

protocol P5 {
  associatedtype T where T == G<U.T>
  associatedtype U : P5a where U.U == Int
}