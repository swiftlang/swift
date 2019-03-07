// RUN: not %target-swift-frontend -typecheck %s

protocol P {
  associatedtype A
}

func foo<T: P>(_: () throws -> T) -> T.A? {
  fatalError()
}

_ = foo() { fatalError() } & nil
