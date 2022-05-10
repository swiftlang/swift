// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -disable-requirement-machine-concrete-contraction 2>&1 | %FileCheck %s

protocol P {
  associatedtype T
}

struct G<T> : P {}


protocol Q {
  associatedtype A : P
}

protocol R {}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q1@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.[Q]A == G<Self.[Q]A.[P]T>, Self.[Q1]B : P, Self.[Q]A.[P]T == Self.[Q1]B.[P]T>

// GSB: Non-canonical requirement
protocol Q1 : Q {
  associatedtype B : P where A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q1a@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.[Q]A == G<Self.[Q]A.[P]T>, Self.[Q1a]B : P, Self.[Q]A.[P]T : R, Self.[Q]A.[P]T == Self.[Q1a]B.[P]T>

// GSB: Missing requirement
protocol Q1a : Q {
  associatedtype B : P where A.T : R, A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q1b@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.[Q]A == G<Self.[Q]A.[P]T>, Self.[Q1b]B : P, Self.[Q]A.[P]T : R, Self.[Q]A.[P]T == Self.[Q1b]B.[P]T>

// GSB: Non-canonical requirement
protocol Q1b : Q {
  associatedtype B : P where B.T : R, A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q2@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.[Q]A == G<Self.[Q]A.[P]T>, Self.[Q2]B : P, Self.[Q]A.[P]T == Self.[Q2]B.[P]T>

// GSB: Missing requirement
protocol Q2 : Q {
  associatedtype B : P where A.T == B.T, A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q3@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.[Q]A == G<Self.[Q]A.[P]T>, Self.[Q3]B : P, Self.[Q]A.[P]T == Self.[Q3]B.[P]T>

// GSB: Unsupported recursive requirement
protocol Q3 : Q {
  associatedtype B : P where A == G<A.T>, A.T == B.T
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q4@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.[Q]A == G<Self.[Q]A.[P]T>, Self.[Q4]B : P, Self.[Q]A.[P]T == Self.[Q4]B.[P]T>

// GSB: Unsupported recursive requirement
protocol Q4 : Q {
  associatedtype B : P where A.T == B.T, A == G<A.T>
}
