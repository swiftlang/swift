// RUN: %target-typecheck-verify-swift

protocol P {
  associatedtype X // expected-note {{protocol requires nested type 'X'}}
}

struct S: P {} // expected-error {{type 'S' does not conform to protocol 'P'}}
// expected-note@-1 {{add stubs for conformance}}

func foo<T: P>(_ x: T) -> T.X {}

func bar(_ x: S) {
  foo(x) // result of foo() is a hole
}
