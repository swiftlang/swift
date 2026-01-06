// RUN: %target-typecheck-verify-swift 


// The restriction is that we don't permit suppression requirements on
// associated types without an experimental feature for that.

protocol P {
  associatedtype A: ~Copyable // expected-error {{cannot suppress 'Copyable' requirement of an associated type}}
}

protocol P_Prime: P {}

protocol Primary<T> {
  associatedtype T: ~Copyable // expected-error {{cannot suppress 'Copyable' requirement of an associated type}}
}

// This is fine, since T isn't an associatedtype, it's substituted into one.
typealias AliasPrimary<T> = Primary<T> where T: ~Copyable

protocol S {
  associatedtype One: ~Copyable // expected-error {{cannot suppress 'Copyable' requirement of an associated type}}

  associatedtype Two: ~Escapable & ~Copyable
                      // expected-error@-1 {{cannot suppress 'Copyable' requirement of an associated type}}
                      // expected-error@-2 {{cannot suppress 'Escapable' requirement of an associated type}}

  associatedtype Three: ~Escapable // expected-error {{cannot suppress 'Escapable' requirement of an associated type}}
}

protocol Base {
    associatedtype A
}
protocol Derived: Base where Self.A: ~Copyable {} // expected-error {{cannot suppress 'Copyable' requirement of an associated type}}

protocol Q {
  associatedtype A where A: ~Copyable // expected-error {{cannot suppress 'Copyable' requirement of an associated type}}
}

protocol R where Self.A: ~Copyable { // expected-error {{cannot suppress 'Copyable' requirement of an associated type}}
  associatedtype A
}
