// RUN: %target-typecheck-verify-swift

protocol Base { // expected-note {{'Base' previously declared here}}
// expected-note@-1 {{found candidate 'Base'}}
  associatedtype E
}

struct Base<T> {} // expected-error {{invalid redeclaration of 'Base'}}
// expected-note@-1 {{found candidate 'Base'}}

protocol Derived : Base { // expected-error {{'Base' is ambiguous for type lookup in this context}}
  associatedtype E
}

func foo<T : Derived>(_: T.E, _: T) {}
