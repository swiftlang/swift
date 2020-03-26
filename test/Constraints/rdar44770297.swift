// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype A
}

func foo<T: P>(_: () throws -> T) -> T.A? {
  fatalError()
}

// TODO(diagnostics): This expression is truly ambiguous because there is no conformance between `Never` and `P`
// which means no associated type `A` and `nil` can't be an argument to any overload of `&` so we end
// up generating at least 3 fixes per overload of `&`. But we could at least point to where the problems are.
let _ = foo() {fatalError()} & nil // expected-error {{type of expression is ambiguous without more context}}
