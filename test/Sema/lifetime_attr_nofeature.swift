// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: asserts

struct NE : ~Escapable { 
}

@lifetime(ne) // expected-error{{'@lifetime' attribute is only valid when experimental feature LifetimeDependence is enabled}} expected-error{{expected declaration}}
func derive(_ ne: NE) -> NE { // expected-error{{returning ~Escapable type requires '-enable-experimental-feature LifetimeDependence'}}
  ne
}
