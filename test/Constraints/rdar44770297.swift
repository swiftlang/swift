// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

func foo<T: P>(_: () throws -> T) -> T.A? { // expected-note {{in call to function 'foo'}}
  fatalError()
}

let _ = foo() {fatalError()} & nil // expected-error {{generic parameter 'T' could not be inferred}}
