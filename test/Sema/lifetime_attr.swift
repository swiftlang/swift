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

func takeInvalidAttrOnNonExistingParam(f: @_lifetime(copy nonexisting) (_ ne: NE) -> NE) {} // expected-error{{invalid parameter name specified 'nonexisting'}}

@_lifetime(copy self) // expected-error{{invalid lifetime dependence specifier on non-existent self}}
func invalidAttrOnNonExistingSelf(_ ne: NE) -> NE {
  ne
}

func takeInvalidAttrOnNonExistingSelf(f: @_lifetime(copy self) (_ ne: NE) -> NE) {} // expected-error{{invalid lifetime dependence specifier on non-existent self}}

@_lifetime(2) // expected-error{{invalid parameter index specified '2'}}
func invalidAttrOnNonExistingParamIndex(_ ne: NE) -> NE {
  ne
}

func takeInvalidAttrOnNonExistingParamIndex(f: @_lifetime(2) (_ ne: NE) -> NE) {} // expected-error{{invalid parameter index specified '2'}}

@_lifetime(copy ne, borrow ne) // expected-error{{duplicate lifetime dependence specifier}}
func invalidDuplicateLifetimeDependence1(_ ne: borrowing NE) -> NE {
  ne
}

func takeInvalidDuplicateLifetimeDependence1(f: @_lifetime(copy ne, borrow ne) (_ ne: borrowing NE) -> NE) {} // expected-error{{duplicate lifetime dependence specifier}}

class Klass {}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with consuming ownership}}
func invalidDependenceConsumeKlass(_ x: consuming Klass) -> NE {
  NE()
}

func takeInvalidDependenceConsumeKlass(f: @_lifetime(borrow x) (_ x: consuming Klass) -> NE) {} // expected-error{{invalid use of borrow dependence with consuming ownership}}

@_lifetime(&x) // expected-error{{invalid use of & dependence with borrowing ownership}}
               // expected-note @-1{{use '@_lifetime(borrow x)' instead}}
func invalidDependenceBorrowKlass(_ x: borrowing Klass) -> NE {
  NE()
}

func takeInvalidDependenceBorrowKlass(f: @_lifetime(&x) (_ x: borrowing Klass) -> NE) {} // expected-error{{invalid use of & dependence with borrowing ownership}}
                                                                                         // expected-note @-1{{use '@_lifetime(borrow x)' instead}}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                     // expected-note @-1{{use '@_lifetime(&x)' instead}}
func invalidDependenceInoutKlass(_ x: inout Klass) -> NE {
  NE()
}

func takeInvalidDependenceInoutKlass(f: @_lifetime(borrow x) (_ x: inout Klass) -> NE) {} // expected-error{{invalid use of borrow dependence with inout ownership}}
                                                                                          // expected-note @-1{{use '@_lifetime(&x)' instead}}

@_lifetime(borrow x) // OK
func invalidDependenceConsumeInt(_ x: consuming Int) -> NE {
  NE()
}

func takeInvalidDependenceConsumeInt(f: @_lifetime(borrow x) (_ x: consuming Int) -> NE) {} // OK

@_lifetime(&x) // expected-error{{invalid use of & dependence with borrowing ownership}}
               // expected-note @-1{{use '@_lifetime(borrow x)' instead}}
func invalidDependenceBorrowInt(_ x: borrowing Int) -> NE {
  NE()
}

func takeInvalidDependenceBorrowInt(f: @_lifetime(&x) (_ x: borrowing Int) -> NE) {} // expected-error{{invalid use of & dependence with borrowing ownership}}
                                                                                     // expected-note @-1{{use '@_lifetime(borrow x)' instead}}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                     // expected-note @-1{{use '@_lifetime(&x)' instead}}
func invalidDependenceInoutInt(_ x: inout Int) -> NE {
  NE()
}

func takeInvalidDependenceInoutInt(f: @_lifetime(borrow x) (_ x: inout Int) -> NE) {} // expected-error{{invalid use of borrow dependence with inout ownership}}
                                                                                      // expected-note @-1{{use '@_lifetime(&x)' instead}}

@_lifetime(result: copy source1) // expected-error{{invalid duplicate target lifetime dependencies on function}}
@_lifetime(result: copy source2)
func invalidTarget(_ result: inout NE, _ source1: consuming NE, _ source2: consuming NE) {
  result = source1
}

func takeInvalidTarget(
  f: @_lifetime(result: copy source1) @_lifetime(result: copy source2) // expected-error{{duplicate attribute}}
    (_ result: inout NE, _ source1: consuming NE, _ source2: consuming NE) -> ()) {}

@_lifetime(result: copy source)   // expected-error{{invalid duplicate target lifetime dependencies on function}}
@_lifetime(result: borrow source)
func invalidSource(_ result: inout NE, _ source: consuming NE) {
  result = source
}

// NOTE: There can be no meaningful function type counterpart to invalidSource since function types can only have one @_lifetime attribute.

@_lifetime(immortal)
func immortalConflict(_ immortal: Int) -> NE { // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}
  NE()
}

func takeImmortalConflict(f: @_lifetime(immortal) (_ immortal: Int) -> NE) {} // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}

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

func takeTestParameterDep(f: @_lifetime(span: borrow holder) (_ holder: AnyObject, _ span: Span<Int>) -> ()) {} // expected-error{{lifetime-dependent parameter 'span' must be 'inout'}}

@_lifetime(&ne)
func inoutLifetimeDependence(_ ne: inout NE) -> NE {
  ne
}

func takeInoutLifetimeDependence(f: @_lifetime(&ne) (_ ne: inout NE) -> NE) {}

@_lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(&k)' instead}}
func dependOnEscapable(_ k: inout Klass) -> NE {
  NE()
}

func takeDependOnEscapable(f: @_lifetime(copy k) (_ k: inout Klass) -> NE) {} // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(&k)' instead}}

@_lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(borrow k)' instead}}
func dependOnEscapable(_ k: borrowing Klass) -> NE { 
  NE()
}

func takeDependOnEscapable(f: @_lifetime(copy k) (_ k: borrowing Klass) -> NE) {} // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(borrow k)' instead}}

@_lifetime(copy k) // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}
func dependOnEscapable(_ k: consuming Klass) -> NE { 
  NE()
}

func takeDependOnEscapable(f: @_lifetime(copy k) (_ k: consuming Klass) -> NE) {} // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}

struct Wrapper : ~Escapable {
  var _ne: NE

  var ne: NE {
    @_lifetime(copy self)
    get {
      _ne
    }
    @_lifetime(self: &self)
    nonmutating _modify {// expected-error{{lifetime-dependent parameter 'self' must be 'inout'}}
      // expected-error@-1{{cannot infer the lifetime dependence scope on a method with a ~Escapable parameter, specify '@_lifetime(borrow self)' or '@_lifetime(copy self)'}}
    }
  }

  var otherNE: NE {
    @_lifetime(copy self)
    get {
      _ne
    }
    @_lifetime(self: borrow newValue)
    set {
      self._ne = newValue
    }
    @_lifetime(&self)
    _modify {
      yield &self._ne
    }
  }
}

@_lifetime(inValue) // expected-error{{invalid lifetime dependence on an Escapable result}}
func getInt(_ inValue: Int) -> Int {
  return inValue
}

func takeGetInt(f: @_lifetime(inValue) (_ inValue: Int) -> Int) {} // expected-error{{invalid lifetime dependence on an Escapable result}}

@_lifetime(_outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
func getInt(_outValue: inout Int, _ inValue: Int)  {
  _outValue = inValue
}

func takeGetInt(f: @_lifetime(outValue: borrow inValue) (_ outValue: inout Int, _ inValue: Int) -> ()) {} // expected-error{{invalid lifetime dependence on an Escapable target}}

@_lifetime(inValue) // expected-error{{invalid lifetime dependence on an Escapable result}}
func getGeneric<T>(_ inValue: T) -> T {
  return inValue
}

func takeGetGeneric<T>(f: @_lifetime(inValue) (_ inValue: T) -> T) {} // expected-error{{invalid lifetime dependence on an Escapable result}}

@_lifetime(_outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
func getGeneric<T>(_outValue: inout T, _ inValue: T)  {
  _outValue = inValue
}

func takeGetGeneric<T>(f: @_lifetime(outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
                         (_ outValue: inout T, _ inValue: T) -> ()) {}

@_lifetime(borrow inValue)
func getGeneric<T : ~Escapable>(_ inValue: T) -> T {
  return inValue
}

// TODO: rdar://160894371?
func takeGetGenericImplicit<T : ~Escapable>(f: @_lifetime(borrow inValue) (_ inValue: T) -> T) {} // expected-error{{invalid use of borrow dependence with context-dependent ownership}}

func takeGetGeneric<T : ~Escapable>(f: @_lifetime(borrow inValue) (_ inValue: borrowing T) -> T) {}

@_lifetime(outValue: borrow inValue)
func getGeneric<T : ~Escapable>(_ outValue: inout T, _ inValue: T)  {
  outValue = inValue
}

@_lifetime(o: borrow i)
func takeGetGenericAndArgs<T: ~Escapable>(f: @_lifetime(outValue: borrow inValue)
                                            (_ outValue: inout T, _ inValue: borrowing T) -> (), o: inout T, i: T) {
  f(&o, i)
}

func takeGetGenericCorrect<T : ~Escapable>(f: @_lifetime(outValue: borrow inValue) (_ outValue: inout T, _ inValue: borrowing T) -> ()) {}

do {
  let x = NE()
  var y = NE()

  takeGetGenericAndArgs(f: getGeneric, o: &y, i: x)

  let inferredTypeFn = inoutLifetimeDependence
  let unannotatedFn: (inout NE) -> NE = inoutLifetimeDependence // expected-error{{value of type 'NE' does not conform to specified type 'Escapable'}}
  // TODO: rdar://160894371
  let annotatedFn: @_lifetime(&ne) (_ ne: inout NE) -> NE = inoutLifetimeDependence // expected-error{{value of type 'NE' does not conform to specified type 'Escapable'}}

  // TODO: Closure sema tests
  // takeGetGenericCorrect { @_lifetime(outV: borrow inV) (outV: inout NE, inV: NE) in
    // outV = inV
  // }
}

func nonexistentSelfInFunctionType(f: @_lifetime(copy self) () -> NE) {} // expected-error{{invalid lifetime dependence specifier on non-existent self}}
