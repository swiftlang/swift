// RUN: %target-typecheck-verify-swift -swift-version 4

protocol P {
  associatedtype A
  associatedtype B

  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B // expected-error{{instance method requirement 'f' cannot add constraint 'Self.A == Self.B' on 'Self'}}
  // expected-note@-1 {{protocol requires function 'f' with type '<T> (T) -> ()'}}
}

extension P {
  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B { }
  // expected-note@-1 {{candidate would match if 'X.A' (aka 'X') was the same type as 'X.B' (aka 'Int')}}
}

struct X : P { 
  // expected-error@-1 {{type 'X' does not conform to protocol 'P'}}
  // expected-note@-2 {{add stubs for conformance}}
  typealias A = X
  typealias B = Int
}

protocol P2 {
  associatedtype A

  func f<T: P2>(_: T) where T.A == Self.A, T.A: P2 // expected-error{{instance method requirement 'f' cannot add constraint 'Self.A: P2' on 'Self'}}
}

class C { }

protocol P3 {
  associatedtype A

  func f<T: P3>(_: T) where T.A == Self.A, T.A: C // expected-error{{instance method requirement 'f' cannot add constraint 'Self.A: C' on 'Self'}}
  func g<T: P3>(_: T) where T.A: C, T.A == Self.A  // expected-error{{instance method requirement 'g' cannot add constraint 'Self.A: C' on 'Self'}}
}

protocol Base {
  associatedtype Assoc
}

protocol Sub1: Base {
  associatedtype SubAssoc: Assoc
  // expected-error@-1 {{type 'Self.SubAssoc' constrained to non-protocol, non-class type 'Self.Assoc'}}
}

// FIXME: This error is incorrect in what it states.
protocol Sub2: Base {
  associatedtype SubAssoc where SubAssoc: Assoc // expected-error {{type 'Self.SubAssoc' constrained to non-protocol, non-class type 'Self.Assoc'}}
}

struct S {}

// FIX-ME: One of these errors is redundant.
protocol P4 {
  associatedtype X : S
  // expected-error@-1 {{type 'Self.X' constrained to non-protocol, non-class type 'S'}}
}

protocol P5 {
  associatedtype Y where Y : S // expected-error {{type 'Self.Y' constrained to non-protocol, non-class type 'S'}}
}

protocol P6 {
  associatedtype T
  associatedtype U

  func foo() where T == U
  // expected-error@-1 {{instance method requirement 'foo()' cannot add constraint 'Self.T == Self.U' on 'Self'}}
  // expected-note@-2 {{protocol requires function 'foo()' with type '() -> ()'}}
}

struct S2 : P6 {
  // expected-error@-1 {{type 'S2' does not conform to protocol 'P6'}}
  // expected-note@-2 {{add stubs for conformance}}
  typealias T = Int
  typealias U = String

  func foo() {}
  // expected-note@-1 {{candidate would match if 'S2.T' (aka 'Int') was the same type as 'S2.U' (aka 'String')}}

  // FIXME: This error is bogus and should be omitted on account of the protocol requirement itself
  // being invalid.
}

// This used to emit a diagnostic with a canonical type in it.
protocol P7 {
  associatedtype A
  func f() // expected-note {{protocol requires function 'f()' with type '() -> ()'}}
}

extension P7 where A: Equatable {
  func f() {} // expected-note {{candidate would match if 'C7<T>.A' (aka 'T') conformed to 'Equatable'}}
}

class C7<T>: P7 { // expected-error {{type 'C7<T>' does not conform to protocol 'P7'}}
// expected-note@-1 {{add stubs for conformance}}
  typealias A = T
}

// This used to just crash.
protocol LayoutConstraint {
  associatedtype A

  func f<T>(_: T) where A: AnyObject
  // expected-error@-1 {{instance method requirement 'f' cannot add constraint 'Self.A: AnyObject' on 'Self'}}
}

protocol Q {
  associatedtype A3
}

// We missed these cases originally.
protocol ComplexDerivation {
  associatedtype A1
  associatedtype A2: Q

  func bad1<B: Equatable>(_: B) where B == Self.A1
  // expected-warning@-1 {{instance method requirement 'bad1' cannot add constraint 'Self.A1: Equatable' on 'Self'; this will be an error in a future Swift language mode}}

  func bad2<B>(_: B) where A1 == [B]
  // expected-warning@-1 {{instance method requirement 'bad2' cannot add constraint 'Self.A1 == [B]' on 'Self'; this will be an error in a future Swift language mode}}

  func good<B>(_: B) where A2 == B  // This is fine

  func bad3<B, C>(_: B, _: C) where A2 == B, B.A3 == [C]
  // expected-warning@-1 {{instance method requirement 'bad3' cannot add constraint 'Self.A2.A3 == Array<C>' on 'Self'; this will be an error in a future Swift language mode}}
}
