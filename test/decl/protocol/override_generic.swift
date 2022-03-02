// RUN: %target-typecheck-verify-swift

protocol Base {
  func foo1<T : P>(_: T, _: T.T)
  func foo2<T : P>(_: T, _: T.T)
}

protocol Derived : Base {
  func foo1<T : P>(_: T, _: T.T)
  func foo2<T : Q>(_: T, _: T.T)
}

protocol P {
  associatedtype T
}

protocol Q {
  associatedtype T
}
