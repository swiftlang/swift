// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

func foo<T: P>(_: () throws -> T) -> T.A? {
  fatalError()
}

let _ = foo() {fatalError()} & nil // expected-error {{value of optional type 'Never.A?' must be unwrapped to a value of type 'Never.A'}}
// expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
// expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
