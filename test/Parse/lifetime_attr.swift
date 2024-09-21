// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature NonescapableTypes 
// REQUIRES: asserts

struct NE : ~Escapable {

}

@lifetime(ne)
func derive(_ ne: NE) -> NE {
  ne
}

@lifetime // expected-error{{expected '(' after lifetime dependence specifier}} 
func testMissingLParenError(_ ne: NE) -> NE {
  ne
}

@lifetime() // expected-error{{expected identifier, index or self in lifetime dependence specifier}}
func testMissingDependence(_ ne: NE) -> NE {
  ne
}

