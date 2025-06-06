// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: asserts

struct NE : ~Escapable { // expected-error{{an implicit initializer cannot return a ~Escapable result}}
}

@_lifetime(copy ne) // expected-error{{'@_lifetime' attribute is only valid when experimental feature Lifetimes is enabled}}
func derive(_ ne: NE) -> NE { // expected-error{{a function cannot return a ~Escapable result}}
  ne
}
