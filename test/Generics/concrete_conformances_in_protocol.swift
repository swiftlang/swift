// RUN: %target-typecheck-verify-swift -warn-redundant-requirements
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P {
  associatedtype T
}
struct S : P {
  typealias T = S
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).R0@
// CHECK-LABEL: Requirement signature: <Self where Self.[R0]A == S>

protocol R0 {
  associatedtype A where A : P, A == S
  // expected-warning@-1 {{redundant conformance constraint 'S' : 'P'}}
}

////

struct G<T> : P {}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).R1@
// CHECK-LABEL: Requirement signature: <Self where Self.[R1]B == G<Self.[R1]A>>

protocol R1 {
  associatedtype A
  associatedtype B where B : P, B == G<A>
  // expected-warning@-1 {{redundant conformance constraint 'G<Self.A>' : 'P'}}
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).R2@
// CHECK-LABEL: Requirement signature: <Self where Self.[R2]A == G<Self.[R2]B>>

protocol R2 {
  associatedtype A where A : P, A == G<B>
  // expected-warning@-1 {{redundant conformance constraint 'G<Self.B>' : 'P'}}
  associatedtype B
}

////

protocol PP {
  associatedtype T : P
}

struct GG<T : P> : PP {}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR3@
// CHECK-LABEL: Requirement signature: <Self where Self.[RR3]A : P, Self.[RR3]B == GG<Self.[RR3]A>>

protocol RR3 {
  associatedtype A : P
  associatedtype B where B : PP, B == GG<A>
  // expected-warning@-1 {{redundant conformance constraint 'GG<Self.A>' : 'PP'}}
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR4@
// CHECK-LABEL: Requirement signature: <Self where Self.[RR4]A == GG<Self.[RR4]B>, Self.[RR4]B : P>

protocol RR4 {
  associatedtype A where A : PP, A == GG<B>
  // expected-warning@-1 {{redundant conformance constraint 'GG<Self.B>' : 'PP'}}
  associatedtype B : P
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR5@
// CHECK-LABEL: Requirement signature: <Self where Self.[RR5]A : PP, Self.[RR5]B == GG<Self.[RR5]A.[PP]T>>

protocol RR5 {
  associatedtype A : PP
  associatedtype B where B : PP, B == GG<A.T>
  // expected-warning@-1 {{redundant conformance constraint 'GG<Self.A.T>' : 'PP'}}
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).RR6@
// CHECK-LABEL: Requirement signature: <Self where Self.[RR6]A == GG<Self.[RR6]B.[PP]T>, Self.[RR6]B : PP>

protocol RR6 {
  associatedtype A where A : PP, A == GG<B.T>
  // expected-warning@-1 {{redundant conformance constraint 'GG<Self.B.T>' : 'PP'}}
  associatedtype B : PP
}

protocol P1 {
  associatedtype T : P1
}

struct GGG<U : P1> : P1 {
  typealias T = GGG<GGG<U>>
}

// CHECK-LABEL: concrete_conformances_in_protocol.(file).P2@
// CHECK-LABEL: Requirement signature: <Self where Self.[P2]T == GGG<Self.[P2]U>, Self.[P2]U : P1>

protocol P2 {
  associatedtype T : P1 where T == GGG<U>
  // expected-warning@-1 {{redundant conformance constraint 'GGG<Self.U>' : 'P1'}}
  associatedtype U : P1
}

