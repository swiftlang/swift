// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -disable-requirement-machine-concrete-contraction

protocol P {
  associatedtype T
}

struct G<T> : P {}


protocol Q {
  associatedtype A : P
}

protocol R {}

// GSB: Non-canonical requirement
// expected-error@+1 {{same-type constraint 'Self.A' == 'G<Self.A.T>' is recursive}}
protocol Q1 : Q {
  associatedtype B : P where A == G<B.T>
  // expected-error@-1 {{'T' is not a member type of type 'Self.B'}}
}

// This used to crash
func useQ1<T : Q1>(_: T) -> T.A.T.Type {
  // expected-error@-1 {{'A' is not a member type of type 'T'}}
  return T.A.T.Type
}

// GSB: Missing requirement
// expected-error@+1 {{same-type constraint 'Self.A' == 'G<Self.A.T>' is recursive}}
protocol Q1a : Q {
  associatedtype B : P where A.T : R, A == G<B.T>
  // expected-error@-1 {{'T' is not a member type of type 'Self.B'}}
}

// GSB: Non-canonical requirement
// expected-error@+1 {{same-type constraint 'Self.A' == 'G<Self.A.T>' is recursive}}
protocol Q1b : Q {
  associatedtype B : P where B.T : R, A == G<B.T>
  // expected-error@-1 2{{'T' is not a member type of type 'Self.B'}}
}

// GSB: Missing requirement
// expected-error@+1 {{same-type constraint 'Self.A' == 'G<Self.A.T>' is recursive}}
protocol Q2 : Q {
  associatedtype B : P where A.T == B.T, A == G<B.T>
  // expected-error@-1 2{{'T' is not a member type of type 'Self.B'}}
}

// GSB: Unsupported recursive requirement
// expected-error@+1 {{same-type constraint 'Self.A' == 'G<Self.A.T>' is recursive}}
protocol Q3 : Q {
  associatedtype B : P where A == G<A.T>, A.T == B.T
  // expected-error@-1 {{'T' is not a member type of type 'Self.B'}}
}

// GSB: Unsupported recursive requirement
// expected-error@+1 {{same-type constraint 'Self.A' == 'G<Self.A.T>' is recursive}}
protocol Q4 : Q {
  associatedtype B : P where A.T == B.T, A == G<A.T>
  // expected-error@-1 {{'T' is not a member type of type 'Self.B'}}
}
