// RUN: %target-typecheck-verify-swift -requirement-machine=verify

protocol P {
  associatedtype T where T == U?
  associatedtype U
}

func sameType<T>(_: T.Type, _: T.Type) {}

func foo<X : P, Y : P>(_: X, _: Y) {
  // X.T is Optional<X.U>.
  sameType(X.T.self, X.U?.self)

  // Y.T is Optional<Y.U>.
  sameType(Y.T.self, Y.U?.self)
}

protocol Q {
  associatedtype T where T == Self?
}

func foo<X : Q, Y : Q>(_: X, _: Y) {
  // X.T is Optional<X>.
  sameType(X.T.self, X?.self)

  // Y.T is Optional<Y>.
  sameType(Y.T.self, Y?.self)
}
