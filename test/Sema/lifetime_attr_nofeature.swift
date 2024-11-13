// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: asserts

struct NE : ~Escapable { // expected-error{{type 'Escapable' requires -enable-experimental-feature NonescapableTypes}}

}

@lifetime(ne) // expected-error{{'@lifetime' attribute is only valid when experimental feature NonescapableTypes is enabled}} expected-error{{expected declaration}}
func derive(_ ne: NE) -> NE {
  ne
}
