// RUN: %target-parse-verify-swift

protocol P {
  associatedtype A
  associatedtype B

  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B // expected-error{{instance method requirement 'f' cannot add constraint 'Self.A == Self.B' on 'Self'}}
}

extension P {
  func f<T: P>(_: T) where T.A == Self.A, T.A == Self.B { }
}

struct X : P {
  typealias A = X
  typealias B = Int
}

protocol P2 {
  associatedtype A

  func f<T: P2>(_: T) where T.A == Self.A, T.A: P2 // expected-error{{instance method requirement 'f' cannot add constraint 'Self.A: P2' on 'Self'}}
}
