// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=verify 2>&1 | %FileCheck %s

protocol P {
  associatedtype T
}
struct S : P {
  typealias T = S
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).R0@
// CHECK-LABEL: Requirement signature: <Self where Self.A == S>

protocol R0 {
  associatedtype A where A : P, A == S
}

////

struct G<T> : P {}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).R1@
// CHECK-LABEL: Requirement signature: <Self where Self.B == G<Self.A>>

protocol R1 {
  associatedtype A
  associatedtype B where B : P, B == G<A>
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).R2@
// CHECK-LABEL: Requirement signature: <Self where Self.A == G<Self.B>>

protocol R2 {
  associatedtype A where A : P, A == G<B>
  associatedtype B
}

////

protocol PP {
  associatedtype T : P
}

struct GG<T : P> : PP {}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR3@
// CHECK-LABEL: Requirement signature: <Self where Self.A : P, Self.B == GG<Self.A>>

protocol RR3 {
  associatedtype A : P
  associatedtype B where B : PP, B == GG<A>
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR4@
// CHECK-LABEL: Requirement signature: <Self where Self.A == GG<Self.B>, Self.B : P>

protocol RR4 {
  associatedtype A where A : PP, A == GG<B>
  associatedtype B : P
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR5@
// CHECK-LABEL: Requirement signature: <Self where Self.A : PP, Self.B == GG<Self.A.T>>

protocol RR5 {
  associatedtype A : PP
  associatedtype B where B : PP, B == GG<A.T>
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR6@
// CHECK-LABEL: Requirement signature: <Self where Self.A == GG<Self.B.T>, Self.B : PP>

protocol RR6 {
  associatedtype A where A : PP, A == GG<B.T>
  associatedtype B : PP
}
