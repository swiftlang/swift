// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/50566

struct Foo<T> {}

protocol P1 {
    associatedtype A // expected-note {{protocol requires nested type 'A'}}
}
extension Foo: P1 where A : P1 {}
// expected-error@-1 {{extension of generic struct 'Foo' has self-referential generic requirements}}
// expected-note@-2 {{while resolving type 'A'}}
// expected-note@-3 2{{through reference here}}
// expected-error@-4 {{type 'Foo<T>' does not conform to protocol 'P1'}}
// expected-note@-5 {{add stubs for conformance}} 
