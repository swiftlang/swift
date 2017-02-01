// RUN: %target-typecheck-verify-swift -swift-version 3

protocol P {
  associatedtype A
  associatedtype B

  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B // expected-warning{{adding constraint 'Self.A == Self.B' on 'Self' via instance method requirement 'f' is deprecated}}
  // expected-note@-1{{protocol requires function 'f' with type '<T> (T) -> ()'; do you want to add a stub?}}
}

extension P {
  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B { } // expected-note{{candidate has non-matching type '<Self, T> (T) -> ()'}}
}

struct X : P { // expected-error{{type 'X' does not conform to protocol 'P'}}
  typealias A = X
  typealias B = Int
}

struct Y : P {
  typealias A = Y
  typealias B = Y
}

