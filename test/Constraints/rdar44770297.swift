// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

func foo<T: P>(_: () throws -> T) -> T.A? { // expected-note {{where 'T' = 'Never'}}
  fatalError()
}

let _ = foo() {fatalError()} & nil // expected-error {{global function 'foo' requires that 'Never' conform to 'P'}}
// expected-error@-1 {{value of optional type 'Never.A?' must be unwrapped to a value of type 'Never.A'}}
// expected-note@-2 {{force-unwrap}}
// expected-note@-3 {{coalesce using '??'}}
