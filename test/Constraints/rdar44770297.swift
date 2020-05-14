// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

func foo<T: P>(_: () throws -> T) -> T.A? {
  fatalError()
}

let _ = foo() {fatalError()} & nil // expected-error {{binary operator '&' cannot be applied to operands of type 'Never.A?' and '_'}}
