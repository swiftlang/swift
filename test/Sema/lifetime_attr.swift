// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

struct NE : ~Escapable {
  @lifetime(copy self) // expected-error{{invalid lifetime dependence specifier on non-existent self}}
  init() {}
}

@lifetime(copy nonexisting) // expected-error{{invalid parameter name specified 'nonexisting'}}
func invalidAttrOnNonExistingParam(_ ne: NE) -> NE {
  ne
}

@lifetime(copy self) // expected-error{{invalid lifetime dependence specifier on non-existent self}}
func invalidAttrOnNonExistingSelf(_ ne: NE) -> NE {
  ne
}

@lifetime(2) // expected-error{{invalid parameter index specified '2'}}
func invalidAttrOnNonExistingParamIndex(_ ne: NE) -> NE {
  ne
}

@lifetime(copy ne, borrow ne) // expected-error{{duplicate lifetime dependence specifier}}
func invalidDuplicateLifetimeDependence1(_ ne: borrowing NE) -> NE {
  ne
}

class Klass {}

@lifetime(borrow x) // expected-error{{invalid use of borrow dependence with consuming ownership}}
func invalidDependence(_ x: consuming Klass) -> NE {
  NE()
}

@lifetime(result: copy source) 
@lifetime(result: borrow source) // TODO: display error here
func invalidTarget(_ result: inout NE, _ source: consuming NE) { // expected-error{{invalid duplicate target lifetime dependencies on function}}
  result = source
}

@lifetime(immortal)
func immortalConflict(_ immortal: Int) -> NE { // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}
  NE()
}

do {
  struct Test: ~Escapable {
    var v1: Int
    var v2: NE
  }

  _ = \Test.v1 // expected-error {{key path cannot refer to nonescapable type 'Test'}}
  _ = \Test.v2 // expected-error {{key path cannot refer to nonescapable type 'Test'}} expected-error {{key path cannot refer to nonescapable type 'NE'}}

  func use(t: Test) {
    t[keyPath: \.v1] // expected-error {{key path cannot refer to nonescapable type 'Test'}}
    t[keyPath: \.v2] // expected-error {{key path cannot refer to nonescapable type 'Test'}} expected-error {{key path cannot refer to nonescapable type 'NE'}}
  }
}

// rdar://146401190 ([nonescapable] implement non-inout parameter dependencies)
@lifetime(span: borrow holder)
func testParameterDep(holder: AnyObject, span: Span<Int>) {}  // expected-error{{lifetime-dependent parameter must be 'inout'}}

@lifetime(&ne)
func inoutLifetimeDependence(_ ne: inout NE) -> NE {
  ne
}

@lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(&k)' instead}}
func dependOnEscapable(_ k: inout Klass) -> NE {
  NE()
}

@lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@lifetime(borrow k)' instead}}
func dependOnEscapable(_ k: borrowing Klass) -> NE { 
  NE()
}

@lifetime(copy k) // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}
func dependOnEscapable(_ k: consuming Klass) -> NE { 
  NE()
}

