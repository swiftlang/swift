// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature Lifetimes 

// REQUIRES: swift_feature_Lifetimes

struct E {}

struct NE : ~Escapable {}

@_lifetime(copy ne)
func derive(_ ne: NE) -> NE {
  ne
}

@_lifetime(borrow ne1, copy ne2)
func derive(_ ne1: NE, _ ne2: NE) -> NE {
  if (Int.random(in: 1..<100) < 50) {
    return ne1
  }
  return ne2
}

@_lifetime // expected-error{{expected '(' after lifetime dependence specifier}}
func testMissingLParenError(_ ne: NE) -> NE { // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
  ne
}

@_lifetime() // expected-error{{expected 'copy', 'borrow', or '&' followed by an identifier, index or 'self' in lifetime dependence specifier}}
func testMissingDependence(_ ne: NE) -> NE { // expected-error{{cannot infer the lifetime dependence scope on a function with a ~Escapable parameter, specify '@_lifetime(borrow ne)' or '@_lifetime(copy ne)'}}
  ne
}

@_lifetime(borrow borrow)
func testNameConflict(_ borrow: E) -> NE {
  NE()
}
