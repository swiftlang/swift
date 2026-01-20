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

typealias InvalidAttrOnNonExistingParamType = @_lifetime(copy nonexisting) (_ ne: NE) -> NE // expected-error{{invalid parameter name specified 'nonexisting'}}

@_lifetime(copy self) // expected-error{{invalid lifetime dependence specifier on non-existent self}}
func invalidAttrOnNonExistingSelf(_ ne: NE) -> NE {
  ne
}

typealias InvalidAttrOnNonExistingSelfType = @_lifetime(copy self) (_ ne: NE) -> NE // expected-error{{invalid lifetime dependence specifier on non-existent self}}

let closureInvalidAttrOnNonExistingSelf =
  @_lifetime(copy self) // expected-error{{invalid lifetime dependence specifier on non-existent self}}
  (_ ne: NE) -> NE {
    ne
  }

@_lifetime(2) // expected-error{{invalid parameter index specified '2'}}
func invalidAttrOnNonExistingParamIndex(_ ne: NE) -> NE {
  ne
}

typealias InvalidAttrOnNonExistingParamIndexType = @_lifetime(2) (_ ne: NE) -> NE // expected-error{{invalid parameter index specified '2'}}

let closureInvalidAttrOnNonExistingParamIndex =
  @_lifetime(2) // expected-error{{invalid parameter index specified '2'}}
  (_ ne: NE) -> NE {
    ne
  }

@_lifetime(copy ne, borrow ne) // expected-error{{duplicate lifetime dependence specifier}}
func invalidDuplicateLifetimeDependence(_ ne: borrowing NE) -> NE {
  ne
}

typealias InvalidDuplicateLifetimeDependenceType = @_lifetime(copy ne, borrow ne) (_ ne: borrowing NE) -> NE // expected-error{{duplicate lifetime dependence specifier}}

let closureInvalidDuplicateLifetimeDependence1 =
  @_lifetime(copy ne, borrow ne) // expected-error{{duplicate lifetime dependence specifier}}
  (_ ne: borrowing NE) -> NE {
    ne
  }

class Klass {}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with consuming ownership}}
func invalidDependenceConsumeKlass(_ x: consuming Klass) -> NE {
  NE()
}

typealias InvalidDependenceConsumeKlassType = @_lifetime(borrow x) (_ x: consuming Klass) -> NE // expected-error{{invalid use of borrow dependence with consuming ownership}}

let closureInvalidDependenceConsumeKlass =
  @_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with consuming ownership}}
  (_ x: consuming Klass) -> NE {
    NE()
  }

@_lifetime(&x) // expected-error{{invalid use of & dependence with borrowing ownership}}
               // expected-note @-1{{use '@_lifetime(borrow x)' instead}}
func invalidDependenceBorrowKlass(_ x: borrowing Klass) -> NE {
  NE()
}

typealias InvalidDependenceBorrowKlassType = @_lifetime(&x) (_ x: borrowing Klass) -> NE // expected-error{{invalid use of & dependence with borrowing ownership}}
                                                                                         // expected-note @-1{{use '@_lifetime(borrow x)' instead}}

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                     // expected-note @-1{{use '@_lifetime(&x)' instead}}
func invalidDependenceInoutKlass(_ x: inout Klass) -> NE {
  NE()
}

typealias InvalidDependenceInoutKlassType = @_lifetime(borrow x) (_ x: inout Klass) -> NE // expected-error{{invalid use of borrow dependence with inout ownership}}
                                                                                          // expected-note @-1{{use '@_lifetime(&x)' instead}}

let closureInvalidDependenceInoutKlass =
  @_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                       // expected-note @-1{{use '@_lifetime(&x)' instead}}
  (_ x: inout Klass) -> NE {
    NE()
  }

@_lifetime(borrow x) // OK
func invalidDependenceConsumeInt(_ x: consuming Int) -> NE {
  NE()
}

typealias InvalidDependenceConsumeIntType = @_lifetime(borrow x) (_ x: consuming Int) -> NE // OK

@_lifetime(&x) // expected-error{{invalid use of & dependence with borrowing ownership}}
               // expected-note @-1{{use '@_lifetime(borrow x)' instead}}
func invalidDependenceBorrowInt(_ x: borrowing Int) -> NE {
  NE()
}

typealias InvalidDependenceBorrowIntType = @_lifetime(&x) (_ x: borrowing Int) -> NE // expected-error{{invalid use of & dependence with borrowing ownership}}
                                                                                     // expected-note @-1{{use '@_lifetime(borrow x)' instead}}

let closureInvalidDependenceBorrowInt =
  @_lifetime(&x) // expected-error{{invalid use of & dependence with borrowing ownership}}
                 // expected-note @-1{{use '@_lifetime(borrow x)' instead}}
  (_ x: borrowing Int) -> NE {
    NE()
  }

@_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                     // expected-note @-1{{use '@_lifetime(&x)' instead}}
func invalidDependenceInoutInt(_ x: inout Int) -> NE {
  NE()
}

typealias InvalidDependenceInoutIntType = @_lifetime(borrow x) (_ x: inout Int) -> NE // expected-error{{invalid use of borrow dependence with inout ownership}}
                                                                                      // expected-note @-1{{use '@_lifetime(&x)' instead}}

let closureInvalidDependenceInoutInt =
  @_lifetime(borrow x) // expected-error{{invalid use of borrow dependence with inout ownership}}
                       // expected-note @-1{{use '@_lifetime(&x)' instead}}
  (_ x: inout Int) -> NE {
    NE()
  }

@_lifetime(result: copy source1) // expected-error{{invalid duplicate target lifetime dependencies on function}}
@_lifetime(result: copy source2)
func invalidTarget(_ result: inout NE, _ source1: consuming NE, _ source2: consuming NE) {
  result = source1
}

typealias InvalidTargetType =
  @_lifetime(result: copy source1) // expected-error{{invalid duplicate target lifetime dependencies on function}}
  @_lifetime(result: copy source2)                                 
  (_ result: inout NE, _ source1: consuming NE, _ source2: consuming NE) -> ()

let closureInvalidTarget =
  @_lifetime(result: copy source1) // expected-error{{invalid duplicate target lifetime dependencies on function}}
  @_lifetime(result: copy source2)
  (_ result: inout NE, _ source1: consuming NE, _ source2: consuming NE) {
    result = source1
  }

@_lifetime(result: copy source)   // expected-error{{invalid duplicate target lifetime dependencies on function}}
@_lifetime(result: borrow source)
func invalidSource(_ result: inout NE, _ source: consuming NE) {
  result = source
}

typealias InvalidSourceType =
  @_lifetime(result: copy source) // expected-error{{invalid duplicate target lifetime dependencies on function}}
  @_lifetime(result: borrow source)
  (_ result: inout NE, _ source: consuming NE) -> ()

@_lifetime(immortal)
func immortalConflict(_ immortal: Int) -> NE { // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}
  NE()
}

typealias ImmortalConflictType = @_lifetime(immortal) (_ immortal: Int) -> NE // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}

let closureImmortalConflict =
  @_lifetime(immortal)
  (_ immortal: Int) -> NE { // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}
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

typealias TestParameterDepType = @_lifetime(span: borrow holder) (_ holder: AnyObject, _ span: Span<Int>) -> () // expected-error{{lifetime-dependent parameter 'span' must be 'inout'}}

@_lifetime(&ne)
func inoutLifetimeDependence(_ ne: inout NE) -> NE {
  ne
}

typealias InoutLifetimeDependenceType = @_lifetime(&ne) (_ ne: inout NE) -> NE

let closureInoutLifetimeDependence =
  @_lifetime(&ne)
  (_ ne: inout NE) -> NE {
    ne
  }

@_lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(&k)' instead}}
func dependOnEscapable(_ k: inout Klass) -> NE {
  NE()
}

typealias DependOnEscapableType = @_lifetime(copy k) (_ k: inout Klass) -> NE // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(&k)' instead}}

@_lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(borrow k)' instead}}
func dependOnEscapable(_ k: borrowing Klass) -> NE { 
  NE()
}

typealias DependOnEscapableType = @_lifetime(copy k) (_ k: borrowing Klass) -> NE // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(borrow k)' instead}}

let closureDependOnEscapable =
  @_lifetime(copy k) // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(borrow k)' instead}}
  (_ k: borrowing Klass) -> NE {
    NE()
  }

@_lifetime(copy k) // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}
func dependOnEscapable(_ k: consuming Klass) -> NE { 
  NE()
}

typealias DependOnEscapableType = @_lifetime(copy k) (_ k: consuming Klass) -> NE // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}

let closureDependOnEscapable =
  @_lifetime(copy k) // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}
  (_ k: consuming Klass) -> NE {
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

typealias GetIntType = @_lifetime(inValue) (_ inValue: Int) -> Int // expected-error{{invalid lifetime dependence on an Escapable result}}

let closureGetInt =
  @_lifetime(inValue) // expected-error{{invalid lifetime dependence on an Escapable result}}
  (_ inValue: Int) -> Int {
    return inValue
  }

@_lifetime(_outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
func getInt(_outValue: inout Int, _ inValue: Int)  {
  _outValue = inValue
}

typealias GetIntType = @_lifetime(outValue: borrow inValue) (_ outValue: inout Int, _ inValue: Int) -> () // expected-error{{invalid lifetime dependence on an Escapable target}}

let closureGetInt =
  @_lifetime(_outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
  (_outValue: inout Int, _ inValue: Int)  {
    _outValue = inValue
  }

@_lifetime(inValue) // expected-error{{invalid lifetime dependence on an Escapable result}}
func getGeneric<T>(_ inValue: T) -> T {
  return inValue
}

typealias GetGenericEscapableType<T> = @_lifetime(inValue) (_ inValue: T) -> T // expected-error{{invalid lifetime dependence on an Escapable result}}

let closureGetGeneric =
  @_lifetime(inValue) // expected-error{{invalid lifetime dependence on an Escapable result}}
  <T>(_ inValue: T) -> T {
    return inValue
  }

@_lifetime(_outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
func getGeneric<T>(_outValue: inout T, _ inValue: T)  {
  _outValue = inValue
}

typealias GetGenericEscapableType2<T> =
  @_lifetime(outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
  (_ outValue: inout T, _ inValue: T) -> ()

@_lifetime(borrow inValue)
func getGeneric<T : ~Escapable>(_ inValue: T) -> T {
  return inValue
}

@_lifetime(_outValue: borrow inValue)
func getGeneric<T : ~Escapable>(_outValue: inout T, _ inValue: T)  {
  _outValue = inValue
}

typealias GetGenericNonEscapableType<T: ~Escapable> =
  @_lifetime(borrow inValue) // expected-error{{invalid use of borrow dependence with context-dependent ownership}}
  (_ inValue: T) -> T

typealias GetGenericNonEscapableType2<T: ~Escapable> =
  @_lifetime(borrow inValue) (_ inValue: borrowing T) -> T

@_lifetime(outValue: borrow inValue)
func getGeneric2<T : ~Escapable>(_ outValue: inout T, _ inValue: T)  {
  outValue = inValue
}

@_lifetime(o: borrow i)
func takeGetGenericAndArgs<T: ~Escapable>(f: @_lifetime(outValue: borrow inValue)
                                            (_ outValue: inout T, _ inValue: borrowing T) -> (), o: inout T, i: T) {
  f(&o, i)
}

typealias GetGenericCorrectType = @_lifetime(outValue: borrow inValue) (_ outValue: inout T, _ inValue: borrowing T) -> () {}

do {
  let x = NE()
  var y = NE()

  takeGetGenericAndArgs(f: getGeneric2, o: &y, i: x)

  let inferredTypeFn = inoutLifetimeDependence
  let unannotatedFn: (inout NE) -> NE = inoutLifetimeDependence // expected-error{{value of type 'NE' does not conform to specified type 'Escapable'}}
  let annotatedFn: @_lifetime(&ne) (_ ne: inout NE) -> NE = inoutLifetimeDependence // OK
}

func nonexistentSelfInFunctionType(f: @_lifetime(copy self) () -> NE) {} // expected-error{{invalid lifetime dependence specifier on non-existent self}}
