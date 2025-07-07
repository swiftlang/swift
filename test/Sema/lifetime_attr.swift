// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature Lifetimes

// REQUIRES: swift_feature_Lifetimes

struct NE : ~Escapable {
  @_lifetime(copy self) // expected-error{{invalid lifetime dependence specifier on non-existent self}}
  init() {}
}

@_lifetime(copy nonexisting) // expected-error{{invalid parameter name specified 'nonexisting'}}
func invalidAttrOnNonExistingParam(_ ne: NE) -> NE {
  ne
}

@_lifetime(copy self) // expected-error{{invalid lifetime dependence specifier on non-existent self}}
func invalidAttrOnNonExistingSelf(_ ne: NE) -> NE {
  ne
}

@_lifetime(2) // expected-error{{invalid parameter index specified '2'}}
func invalidAttrOnNonExistingParamIndex(_ ne: NE) -> NE {
  ne
}

@_lifetime(copy ne, borrow ne) // expected-error{{duplicate lifetime dependence specifier}}
func invalidDuplicateLifetimeDependence1(_ ne: borrowing NE) -> NE {
  ne
}

class Klass {}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with consuming ownership}}
func invalidDependenceConsumeKlass(_ x: consuming Klass) -> NE {
  NE()
}

@_lifetime(&x) // expected-error{{invalid use of & dependence with borrowing ownership}}
               // expected-note @-1{{use '@_lifetime(borrow x)' instead}}
func invalidDependenceBorrowKlass(_ x: borrowing Klass) -> NE {
  NE()
}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                     // expected-note @-1{{use '@_lifetime(&x)' instead}}
func invalidDependenceInoutKlass(_ x: inout Klass) -> NE {
  NE()
}

@_lifetime(borrow x) // OK
func invalidDependenceConsumeInt(_ x: consuming Int) -> NE {
  NE()
}

@_lifetime(&x) // expected-error{{invalid use of & dependence with borrowing ownership}}
               // expected-note @-1{{use '@_lifetime(borrow x)' instead}}
func invalidDependenceBorrowInt(_ x: borrowing Int) -> NE {
  NE()
}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                     // expected-note @-1{{use '@_lifetime(&x)' instead}}
func invalidDependenceInoutInt(_ x: inout Int) -> NE {
  NE()
}

@_lifetime(result: copy source) 
@_lifetime(result: borrow source) // TODO: display error here
func invalidTarget(_ result: inout NE, _ source: consuming NE) { // expected-error{{invalid duplicate target lifetime dependencies on function}}
  result = source
}

@_lifetime(immortal)
func immortalConflict(_ immortal: Int) -> NE { // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}
  NE()
}

do {
  struct Test: ~Escapable { // expected-error{{cannot infer implicit initialization lifetime. Add an initializer with '@_lifetime(...)' for each parameter the result depends on}}
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
@_lifetime(span: borrow holder)
func testParameterDep(holder: AnyObject, span: Span<Int>) {}  // expected-error{{lifetime-dependent parameter 'span' must be 'inout'}}

@_lifetime(&ne)
func inoutLifetimeDependence(_ ne: inout NE) -> NE {
  ne
}

@_lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(&k)' instead}}
func dependOnEscapable(_ k: inout Klass) -> NE {
  NE()
}

@_lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(borrow k)' instead}}
func dependOnEscapable(_ k: borrowing Klass) -> NE { 
  NE()
}

@_lifetime(copy k) // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}
func dependOnEscapable(_ k: consuming Klass) -> NE { 
  NE()
}

struct Wrapper : ~Escapable {
  var _ne: NE

  var ne: NE {
    @_lifetime(copy self)
    get {
      _ne
    }
    @_lifetime(self: &self)
    nonmutating _modify {// expected-error{{lifetime-dependent parameter 'self' must be 'inout'}}
    }
  }
}
