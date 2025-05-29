// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: asserts

struct NE : ~Escapable { // expected-error{{an implicit initializer cannot return a ~Escapable result}}
}

@lifetime(copy ne) // expected-error{{'@lifetime' attribute is only valid when experimental feature LifetimeDependence is enabled}} expected-error{{expected declaration}}
func derive(_ ne: NE) -> NE { // expected-error{{a function cannot return a ~Escapable result}}
  ne
}
