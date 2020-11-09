// RUN: %target-typecheck-verify-swift

protocol P1 where A == Never {
  associatedtype A
}
struct S1: P1 {} // OK, A := Never

protocol P2a {
  associatedtype A
}
protocol P2b: P2a where A == Never {}
protocol P2c: P2b {}
struct S2a: P2b {} // OK, A := Never
struct S2b: P2c {} // OK, A := Never

// Fixed type witnesses can reference dependent members.
protocol P3a {
  associatedtype A
  associatedtype B
}
protocol P3b: P3a where A == [B] {}
struct S3: P3b { // OK, A := [Never], B := Never
  typealias B = Never
}

protocol P4 {}
extension P4 {
  typealias B = Self
}
struct S4: P4, P3b {} // OK, A := [S4], B := S4

// Self is a valid fixed type witness.
protocol P5a {
  associatedtype A
}
protocol P5b: P5a where A == Self {}
struct S5<X>: P5b {} // OK, A := S5<X>


protocol P6 where A == Never { // expected-error {{same-type constraint type 'Never' does not conform to required protocol 'P6'}}
  // expected-error@+2 {{same-type constraint type 'Never' does not conform to required protocol 'P6'}}
  // expected-note@+1 {{protocol requires nested type 'A}}
  associatedtype A: P6
}
struct S6: P6 {} // expected-error {{type 'S6' does not conform to protocol 'P6'}}

protocol P7a where A == Never {
  associatedtype A
}
// expected-error@+2 {{'Self.A' cannot be equal to both 'Bool' and 'Never'}}
// expected-note@+1 {{same-type constraint 'Self.A' == 'Never' implied here}}
protocol P7b: P7a where A == Bool {}
struct S7: P7b {}

protocol P8 where A == Bool {
  associatedtype A
}
// expected-error@+2 {{'P7a' requires the types 'S8.A' (aka 'Bool') and 'Never' be equivalent}}
// expected-note@+1 {{requirement specified as 'Self.A' == 'Never' [with Self = S8]}}
struct S8: P8, P7a {}

protocol P9a where A == Never {
  associatedtype A
}
protocol P9b: P9a {
  associatedtype A
}
struct S9a: P9b {} // OK, A := Never
// expected-error@+2 {{'P9a' requires the types 'S9b.A' (aka 'Bool') and 'Never' be equivalent}}
// expected-note@+1 {{requirement specified as 'Self.A' == 'Never' [with Self = S9b]}}
struct S9b: P9b {
  typealias A = Bool
}
struct S9c: P9b { // OK, S9c.A does not contradict Self.A == Never.
  typealias Sugar = Never
  typealias A = Sugar
}

protocol P10 {}
extension P10 {
  typealias A = Bool
}
// FIXME: 'P10 extension.A' should not be considered a viable type witness;
//  instead, the compiler should infer A := Never and synthesize S10.A.
// expected-error@+2 {{'P9a' requires the types 'S10.A' (aka 'Bool') and 'Never' be equivalent}}
// expected-note@+1 {{requirement specified as 'Self.A' == 'Never' [with Self = S10]}}
struct S10: P10, P9a {}

protocol P11a {
  associatedtype A
}
protocol P11b: P11a where A == Never {}
protocol Q11 {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
}
// FIXME: Unrelated protocols with a matching associated type should
//  also be considered when computing a fixed type witness.
// expected-error@+3 {{type 'S11' does not conform to protocol 'Q11'}}
// expected-error@+2 {{type 'S11' does not conform to protocol 'P11a'}}
// expected-error@+1 {{type 'S11' does not conform to protocol 'P11b'}}
struct S11: Q11, P11b {}

protocol P12 where A == B {
  associatedtype A
  associatedtype B
  func foo(arg: A)
}
struct S12: P12 { // OK, A == B == Never
  func foo(arg: Never) {}
}

protocol P13a {
  associatedtype A
  func foo(arg: A)
}
protocol P13b {
  associatedtype B
}
protocol P13c: P13a, P13b where A == B {}
struct S13: P13c { // OK, A == B == Never
  func foo(arg: Never) {}
}
