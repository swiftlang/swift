// RUN: %target-typecheck-verify-swift -swift-version 4

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

class C { }

protocol P3 {
  associatedtype A

  func f<T: P3>(_: T) where T.A == Self.A, T.A: C // expected-error{{instance method requirement 'f' cannot add constraint 'Self.A: C' on 'Self'}}
  func g<T: P3>(_: T) where T.A: C, T.A == Self.A  // expected-error{{instance method requirement 'g' cannot add constraint 'Self.A: C' on 'Self'}}
}
