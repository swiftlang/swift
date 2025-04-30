// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: asserts

struct NE : ~Escapable { // expected-error{{an implicit initializer with a ~Escapable result requires '-enable-experimental-feature LifetimeDependence'}}
}

@lifetime(copy ne) // expected-error{{'@lifetime' attribute is only valid when experimental feature LifetimeDependence is enabled}} expected-error{{expected declaration}}
func derive(_ ne: NE) -> NE { // expected-error{{a function with a ~Escapable result requires '-enable-experimental-feature LifetimeDependence'}}
  ne
}
