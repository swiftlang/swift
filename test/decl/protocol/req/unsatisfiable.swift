// RUN: %target-typecheck-verify-swift -swift-version 4

protocol P {
  associatedtype A
  associatedtype B

  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B // expected-error{{instance method requirement 'f' cannot add constraint 'Self.A == Self.B' on 'Self'}}
  // expected-note@-1 {{protocol requires function 'f' with type '<T> (T) -> ()'; do you want to add a stub?}}
}

extension P {
  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B { }
  // expected-note@-1 {{candidate would match if 'X' was the same type as 'X.B' (aka 'Int')}}
}

struct X : P { // expected-error {{type 'X' does not conform to protocol 'P'}}
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

// FIXME: The first error is redundant and isn't correct in what it states.
// FIXME: This used to /not/ error in Swift 3. It didn't impose any statically-
// enforced requirements, but the compiler crashed if you used anything but the
// same type.
protocol Sub1: Base {
  associatedtype SubAssoc: Assoc
  // expected-error@-1 {{type 'Self.SubAssoc' constrained to non-protocol, non-class type 'Self.Assoc'}}
  // expected-error@-2 {{inheritance from non-protocol, non-class type 'Self.Assoc'}}
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
  // expected-error@-2 {{inheritance from non-protocol, non-class type 'S'}}
}

protocol P5 {
  associatedtype Y where Y : S // expected-error {{type 'Self.Y' constrained to non-protocol, non-class type 'S'}}
}
