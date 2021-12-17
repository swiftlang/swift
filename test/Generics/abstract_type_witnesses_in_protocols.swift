// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

protocol P {
  associatedtype T
}

struct G<T> : P {}


protocol Q {
  associatedtype A : P
}

protocol R {}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q1@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.A == G<Self.A.T>, Self.B : P, Self.A.T == Self.B.T>

// GSB: Non-canonical requirement
protocol Q1 : Q {
  associatedtype B : P where A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q1a@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.A == G<Self.A.T>, Self.B : P, Self.A.T : R, Self.A.T == Self.B.T>

// GSB: Missing requirement
protocol Q1a : Q {
  associatedtype B : P where A.T : R, A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q1b@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.A == G<Self.A.T>, Self.B : P, Self.A.T : R, Self.A.T == Self.B.T>

// GSB: Non-canonical requirement
protocol Q1b : Q {
  associatedtype B : P where B.T : R, A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q2@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.A == G<Self.A.T>, Self.B : P, Self.A.T == Self.B.T>

// GSB: Missing requirement
protocol Q2 : Q {
  associatedtype B : P where A.T == B.T, A == G<B.T>
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q3@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.A == G<Self.A.T>, Self.B : P, Self.A.T == Self.B.T>

// GSB: Unsupported recursive requirement
protocol Q3 : Q {
  associatedtype B : P where A == G<A.T>, A.T == B.T
}

// CHECK-LABEL: abstract_type_witnesses_in_protocols.(file).Q4@
// CHECK-NEXT: Requirement signature: <Self where Self : Q, Self.A == G<Self.A.T>, Self.B : P, Self.A.T == Self.B.T>

// GSB: Unsupported recursive requirement
protocol Q4 : Q {
  associatedtype B : P where A.T == B.T, A == G<A.T>
}
