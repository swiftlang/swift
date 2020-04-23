// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

func foo<T: P>(_: () throws -> T) -> T.A? {
  fatalError()
}

let _ = foo() {fatalError()} & nil // expected-error {{type of expression is ambiguous without more context}}
