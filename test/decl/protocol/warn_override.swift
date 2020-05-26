// RUN: %target-typecheck-verify-swift -warn-implicit-overrides

// Test the warnings about implicit 'override' of protocol members.


protocol P0 {
  associatedtype A // expected-note{{'A' declared here}}

  func foo() // expected-note{{overridden declaration is here}}

  var prop: A { get } // expected-note{{overridden declaration is here}}
}

protocol P1: P0 {
  associatedtype A // expected-warning{{redeclaration of associated type 'A' from protocol 'P0' is better expressed as a 'where' clause on the protocol}}

  func foo() // expected-warning{{implicit override should be marked with 'override' or suppressed with '@_nonoverride'}}

  var prop: A { get } // expected-warning{{implicit override should be marked with 'override' or suppressed with '@_nonoverride'}}
}

// Silence warnings with @_nonoveride.
protocol P2: P0 {
  @_nonoverride
  associatedtype A

  @_nonoverride
  func foo()

  @_nonoverride
  var prop: A { get }
}
