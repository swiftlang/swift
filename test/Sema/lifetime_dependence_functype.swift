// RUN: %target-typecheck-verify-swift -enable-experimental-feature Lifetimes -enable-experimental-feature ClosureLifetimes

// REQUIRES: swift_feature_Lifetimes

struct NC: ~Copyable {
  var ne: NE {
    NE()
  }
}

struct NE: ~Escapable {
  @_unsafeNonescapableResult
  init() {}
}

@_lifetime(copy ne)
func transfer(_ ne: NE) -> NE {
  ne
}

@_lifetime(copy ne)
func applyAnnotatedTransfer(ne: NE, @_lifetime(0) transfer: (NE) -> NE) -> NE { // expected-error{{'@_lifetime' attribute cannot be applied to this declaration}}
  transfer(ne)
}

@_lifetime(copy ne)
func applyCorrectlyAnnotatedTransfer(ne: NE, transfer: @_lifetime(0) (NE) -> NE) -> NE { // OK
  transfer(ne)
}

@_lifetime(copy ne)
func applyTransfer(ne: NE, transfer: (NE) ->  NE) -> NE {
  transfer(ne)
}

func testTransfer(nc: consuming NC) {
  let transferred = applyTransfer(ne: nc.ne, transfer: transfer) // OK

  _ = consume nc
  _ = transfer(transferred)
}

func borrow(_ nc: borrowing NC) -> NE {
  nc.ne
}

@_lifetime(borrow nc)
func applyBorrow(nc: borrowing NC, borrow: (borrowing NC) -> NE) -> NE {
  borrow(nc)
}

func testBorrow(nc: consuming NC) {
  let borrowed = applyBorrow(nc: nc, borrow: borrow) // OK
  _ = consume nc
  _ = transfer(borrowed)
}

// Tests adapted from lifetime_attr.swift for function types.

typealias InvalidAttrOnNonExistingParamType = @_lifetime(copy nonexisting) (_ ne: NE) -> NE // expected-error{{invalid parameter name specified 'nonexisting'}}

typealias InvalidAttrOnNonExistingSelfType = @_lifetime(copy self) (_ ne: NE) -> NE // expected-error{{invalid lifetime dependence specifier on non-existent self}}

typealias InvalidAttrOnNonExistingParamIndexType = @_lifetime(2) (_ ne: NE) -> NE // expected-error{{invalid parameter index specified '2'}}

typealias InvalidDuplicateLifetimeDependenceType = @_lifetime(copy ne, borrow ne) (_ ne: borrowing NE) -> NE // expected-error{{duplicate lifetime dependence specifier}}

typealias InvalidDependenceConsumeKlassType = @_lifetime(borrow x) (_ x: consuming Klass) -> NE // expected-error{{invalid use of borrow dependence with consuming ownership}}

typealias InvalidDependenceBorrowKlassType = @_lifetime(&x) (_ x: borrowing Klass) -> NE // expected-error{{invalid use of & dependence with borrowing ownership}}
                                                                                         // expected-note @-1{{use '@_lifetime(borrow x)' instead}}

typealias InvalidDependenceInoutKlassType = @_lifetime(borrow x) (_ x: inout Klass) -> NE // expected-error{{invalid use of borrow dependence with inout ownership}}
                                                                                          // expected-note @-1{{use '@_lifetime(&x)' instead}}

typealias InvalidDependenceConsumeIntType = @_lifetime(borrow x) (_ x: consuming Int) -> NE // OK

typealias InvalidDependenceBorrowIntType = @_lifetime(&x) (_ x: borrowing Int) -> NE // expected-error{{invalid use of & dependence with borrowing ownership}}
                                                                                     // expected-note @-1{{use '@_lifetime(borrow x)' instead}}

typealias InvalidDependenceInoutIntType = @_lifetime(borrow x) (_ x: inout Int) -> NE // expected-error{{invalid use of borrow dependence with inout ownership}}
                                                                                      // expected-note @-1{{use '@_lifetime(&x)' instead}}

typealias InvalidTargetType =
  @_lifetime(result: copy source1) // expected-error{{invalid duplicate target lifetime dependencies on function}}
  @_lifetime(result: copy source2)                                 
  (_ result: inout NE, _ source1: consuming NE, _ source2: consuming NE) -> ()

typealias InvalidSourceType =
  @_lifetime(result: copy source) // expected-error{{invalid duplicate target lifetime dependencies on function}}
  @_lifetime(result: borrow source)
  (_ result: inout NE, _ source: consuming NE) -> ()

typealias ImmortalConflictType = @_lifetime(immortal) (_ immortal: Int) -> NE // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}

typealias TestParameterDepType = @_lifetime(span: borrow holder) (_ holder: AnyObject, _ span: Span<Int>) -> () // expected-error{{lifetime-dependent parameter 'span' must be 'inout'}}

typealias InoutLifetimeDependenceType = @_lifetime(&ne) (_ ne: inout NE) -> NE

typealias DependOnEscapableType = @_lifetime(copy k) (_ k: inout Klass) -> NE // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(&k)' instead}}

typealias DependOnEscapableType = @_lifetime(copy k) (_ k: borrowing Klass) -> NE // expected-error{{cannot copy the lifetime of an Escapable type, use '@_lifetime(borrow k)' instead}}

typealias DependOnEscapableType = @_lifetime(copy k) (_ k: consuming Klass) -> NE // expected-error{{invalid lifetime dependence on an Escapable value with consuming ownership}}

typealias GetIntType = @_lifetime(inValue) (_ inValue: Int) -> Int // expected-error{{invalid lifetime dependence on an Escapable result}}

typealias GetIntType = @_lifetime(outValue: borrow inValue) (_ outValue: inout Int, _ inValue: Int) -> () // expected-error{{invalid lifetime dependence on an Escapable target}}

typealias GetGenericEscapableType<T> = @_lifetime(inValue) (_ inValue: T) -> T // expected-error{{invalid lifetime dependence on an Escapable result}}

typealias GetGenericEscapableType2<T> =
  @_lifetime(outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
  (_ outValue: inout T, _ inValue: T) -> ()

typealias GetGenericNonEscapableType<T: ~Escapable> =
  @_lifetime(borrow inValue) // expected-error{{invalid use of borrow dependence with context-dependent ownership}}
  (_ inValue: T) -> T

typealias GetGenericNonEscapableType2<T: ~Escapable> =
  @_lifetime(borrow inValue) (_ inValue: borrowing T) -> T // OK

typealias GetGenericCorrectType =
  @_lifetime(outValue: borrow inValue) (_ outValue: inout T, _ inValue: borrowing T) -> () // OK


@_lifetime(outValue: borrow inValue) // OK
func getGeneric2<T : ~Escapable>(_ outValue: inout T, _ inValue: T)  {
  outValue = inValue
}

@_lifetime(o: borrow i) // OK
func takeGetGenericAndArgs<T: ~Escapable>(f: @_lifetime(outValue: borrow inValue)
                                            (_ outValue: inout T, _ inValue: borrowing T) -> (), o: inout T, i: T) {
  f(&o, i)
}

do {
  let x = NE()
  var y = NE()

  takeGetGenericAndArgs(f: getGeneric2, o: &y, i: x) // OK

  let inferredTypeFn = inoutLifetimeDependence // OK
  let unannotatedFn: (inout NE) -> NE = inoutLifetimeDependence // expected-error{{value of type 'NE' does not conform to specified type 'Escapable'}}
  let annotatedFn: @_lifetime(&ne) (_ ne: inout NE) -> NE = inoutLifetimeDependence // OK
}

// rdar://166912068 (Incorrect error when passing a local function with a non-escapable parameter)
struct NEWithSpan {
  var span: RawSpan

  @_lifetime(copy span)
  init(span: RawSpan) {
    self.span = span
  }
}
func takeBody(body: (inout NEWithSpan) -> Void) {}
func checkNestedFunctions() {
  func doIt(ne: inout NEWithSpan) {}
  takeBody(body: doIt)
}
