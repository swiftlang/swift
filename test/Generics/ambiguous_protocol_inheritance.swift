// RUN: %target-typecheck-verify-swift -requirement-machine=verify

protocol Base { // expected-note {{'Base' previously declared here}}
// expected-note@-1 {{found this candidate}}
  associatedtype E
}

struct Base<T> {} // expected-error {{invalid redeclaration of 'Base'}}
// expected-note@-1 {{found this candidate}}

protocol Derived : Base { // expected-error {{'Base' is ambiguous for type lookup in this context}}
  associatedtype E
}

func foo<T : Derived>(_: T.E, _: T) {}
