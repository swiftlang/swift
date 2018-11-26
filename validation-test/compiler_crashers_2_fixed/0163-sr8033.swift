// RUN: %target-typecheck-verify-swift

struct Foo<T> {}

protocol P1 {
    associatedtype A // expected-note {{protocol requires nested type 'A'; do you want to add it?}}
}
extension Foo: P1 where A : P1 {} // expected-error {{unsupported recursion for reference to associated type 'A' of type 'Foo<T>'}}
// expected-error@-1 {{type 'Foo<T>' does not conform to protocol 'P1'}}
