// RUN: %target-typecheck-verify-swift -enable-experimental-feature Lifetimes

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
func applyAnnotatedTransfer(ne: NE, @_lifetime(copy ne) transfer: (_ ne: NE) -> NE) -> NE { // expected-error{{'@_lifetime' attribute cannot be applied to this declaration}}
  transfer(ne)
}

@_lifetime(copy ne)
func applyCorrectlyAnnotatedTransfer(ne: NE, transfer: @_lifetime(copy ne) (_ ne: NE) -> NE) -> NE { // OK
  transfer(ne)
}

@_lifetime(copy ne)
func applyTransfer(ne: NE, transfer: (NE) ->  NE) -> NE { // OK, copy 0 inferred
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
class Klass {}
typealias InvalidAttrOnNonExistingParamType = @_lifetime(copy nonexisting) (_ ne: NE) -> NE // expected-error{{invalid parameter name specified 'nonexisting'}}

typealias InvalidAttrOnNonExistingSelfType = @_lifetime(copy self) (_ ne: NE) -> NE // expected-error{{invalid lifetime dependence specifier on non-existent self}}

typealias InvalidAttrOnExistingParamIndexType = @_lifetime(0) (_ ne: NE) -> NE // expected-error{{expected 'copy', 'borrow', or '&' followed by an identifier or 'self' in lifetime dependence specifier}}

typealias InvalidAttrOnNonExistingParamIndexType = @_lifetime(2) (_ ne: NE) -> NE // expected-error{{expected 'copy', 'borrow', or '&' followed by an identifier or 'self' in lifetime dependence specifier}}

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
  @_lifetime(result: copy source1)
  @_lifetime(result: copy source2) // expected-error{{invalid duplicate target lifetime dependencies on function}}                                 
  (_ result: inout NE, _ source1: consuming NE, _ source2: consuming NE) -> ()

typealias InvalidSourceType =
  @_lifetime(result: copy source)
  @_lifetime(result: borrow source) // expected-error{{invalid duplicate target lifetime dependencies on function}}
  (_ result: inout NE, _ source: consuming NE) -> ()

typealias ImmortalConflictType = @_lifetime(immortal) (_ immortal: Int) -> NE // expected-error{{conflict between the parameter name and 'immortal' contextual keyword}}

typealias TestParameterDepType = @_lifetime(span: borrow holder) (_ holder: AnyObject, _ span: Span<Int>) -> () // expected-error{{lifetime-dependent parameter 'span' must be 'inout'}}

typealias InoutLifetimeDependenceType = @_lifetime(&ne) (_ ne: inout NE) -> NE

typealias DependOnEscapableType1 = @_lifetime(copy k) (_ k: inout Klass) -> NE // expected-error{{cannot copy the lifetime of an Escapable type}}
                                                                               // expected-note@-1{{use '@_lifetime(&k)' instead}}

typealias DependOnEscapableType2 = @_lifetime(copy k) (_ k: borrowing Klass) -> NE // expected-error{{cannot copy the lifetime of an Escapable type}}
                                                                                   // expected-note@-1{{use '@_lifetime(borrow k)' instead}}

typealias DependOnEscapableType3 = @_lifetime(copy k) (_ k: consuming Klass) -> NE // expected-error{{cannot copy the lifetime of an Escapable type}}
                                                                                   // expected-note@-1{{use '@_lifetime(borrow k)' instead}}

typealias GetIntType1 = @_lifetime(inValue) (_ inValue: Int) -> Int // expected-error{{invalid lifetime dependence on an Escapable result}}

typealias GetIntType2 = @_lifetime(outValue: borrow inValue) (_ outValue: inout Int, _ inValue: Int) -> () // expected-error{{invalid lifetime dependence on an Escapable target}}

typealias GetGenericEscapableType<T> = @_lifetime(inValue) (_ inValue: T) -> T // expected-error{{invalid lifetime dependence on an Escapable result}}

typealias GetGenericEscapableType2<T> =
  @_lifetime(outValue: borrow inValue) // expected-error{{invalid lifetime dependence on an Escapable target}}
  (_ outValue: inout T, _ inValue: T) -> ()

typealias GetGenericNonEscapableType2<T: ~Escapable> =
  @_lifetime(borrow inValue) (_ inValue: borrowing T) -> T // OK

typealias GetGenericCorrectType<T: ~Escapable> =
  @_lifetime(outValue: borrow inValue) (_ outValue: inout T, _ inValue: borrowing T) -> () // OK

@_lifetime(outValue: copy inValue) // OK
func getGeneric<T : ~Escapable>(_ outValue: inout T, _ inValue: borrowing T) { // expected-note{{in call to function 'getGeneric'}}
  outValue = inValue
}

@_lifetime(outValue: borrow inValue) // OK
func getGeneric2<T : ~Escapable>(_ outValue: inout T, _ /* borrowing inferred */ inValue: T)  {
  outValue = inValue
}

@_lifetime(outValueI: immortal) // OK
func getImmortalNE(_ outValueI: inout NE, _ inValueI: borrowing NE) {
  outValueI = NE()
}

@_lifetime(o: borrow i) // OK
func takeGetGenericAndArgs<T: ~Escapable>(f: @_lifetime(outValue: borrow inValue)
                                            (_ outValue: inout T, _ inValue: borrowing T) -> (), o: inout T, i: T) {
  f(&o, i)
}

do {
  let x = NE()
  var y = NE()
  takeGetGenericAndArgs(f: getGeneric, o: &y, i: x)
  // expected-error@-1{{cannot convert value of type '(inout T, borrowing T) -> ()' to expected argument type '@_lifetime(outValue: copy outValue, borrow inValue) (_ outValue: inout NE, _ inValue: borrowing NE) -> ()'}}
  // expected-error@-2{{generic parameter 'T' could not be inferred}}
}
do {
  let x = NE()
  var y = NE()
  takeGetGenericAndArgs(f: getGeneric2, o: &y, i: x) // OK
}
do {
  let x = NE()
  var y = NE()
  takeGetGenericAndArgs(f: getImmortalNE, o: &y, i: x)
  //  expected-error@-1{{cannot convert value of type '@_lifetime(0: immortal) (inout NE, borrowing NE) -> ()' to expected argument type '@_lifetime(outValue: copy outValue, borrow inValue) (_ outValue: inout NE, _ inValue: borrowing NE) -> ()'}}
}
do {
  let x = NE()
  var y = NE()
  takeGetGenericAndArgs(f: { $1 = $0 }, o: &y, i: x) // expected-error{{cannot assign to value: '$1' is immutable}}
}
do {
  let x = NE()
  var y = NE()
  takeGetGenericAndArgs(f: { $0 = $1 }, o: &y, i: x) // OK
}
do {
  let _ = transfer // OK
  let _: (NE) -> NE = transfer // OK
  let _: @_lifetime(copy ne) (_ ne: NE) -> NE = transfer // OK
}

// rdar://166912068 (Incorrect error when passing a local function with a non-escapable parameter)
struct NEWithSpan: ~Escapable {
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

// Bail-out cases where lifetime dependence checking cannot run.
struct CNE<T: ~Escapable>: ~Escapable {
    let ne: T
    @_lifetime(copy ne)
    init(ne: T) {
        self.ne = ne
    }
}

@_lifetime(borrow cne)
func copyCNE(cne: CNE<NE>) -> CNE<NE> {
    return cne
}

public let UnboundGenericParamFunctionType : (CNE) -> CNE<NE> = copyCNE                                         // expected-error{{lifetime dependence checking failed due to unknown parameter type}}
                                                                                                                // expected-error@-1{{value of type 'CNE<NE>' does not conform to specified type 'Escapable'}}
public let UnboundGenericParamFunctionTypeAnnotated : @_lifetime(borrow cne) (_ cne: CNE) -> CNE<NE> = copyCNE  // expected-error{{lifetime dependence checking failed due to unknown parameter type}}
                                                                                                                // expected-error@-1{{value of type 'CNE<NE>' does not conform to specified type 'Escapable'}}
public let UnboundGenericResultFunctionType : (CNE<NE>) -> CNE = copyCNE                                        // expected-error{{lifetime dependence checking failed due to unknown result type}}
                                                                                                                // expected-error@-1{{value of type 'CNE<NE>' does not conform to specified type 'Escapable'}}
public let UnboundGenericResultFunctionTypeAnnotated : @_lifetime(borrow cne) (_ cne: CNE<NE>) -> CNE = copyCNE // expected-error{{lifetime dependence checking failed due to unknown result type}}
                                                                                                                // expected-error@-1{{value of type 'CNE<NE>' does not conform to specified type 'Escapable'}}

public let TypeParameterParamFunctionType = copyCNE as (_) -> CNE<NE>                                         // expected-error{{lifetime dependence checking failed due to unknown parameter type}}
                                                                                                              // expected-error@-1{{failed to produce diagnostic for expression}}
public let TypeParameterParamFunctionTypeAnnotated = copyCNE as @_lifetime(borrow cne) (_ cne: _) -> CNE<NE>  // expected-error{{lifetime dependence checking failed due to unknown parameter type}}
                                                                                                              // expected-error@-1{{failed to produce diagnostic for expression}}
public let TypeParameterResultFunctionType = copyCNE as (CNE<NE>) -> _                                        // expected-error{{lifetime dependence checking failed due to unknown result type}}
                                                                                                              // expected-error@-1{{failed to produce diagnostic for expression}}
public let TypeParameterResultFunctionTypeAnnotated = copyCNE as @_lifetime(borrow cne) (_ cne: CNE<NE>) -> _ // expected-error{{lifetime dependence checking failed due to unknown result type}}
                                                                                                              // expected-error@-1{{failed to produce diagnostic for expression}}

// Closure Context Dependence Tests

// This case should pass after we add support for dependencies on the closure context.
// 
// We had to move it out of
// SILOptimizer/lifetime_dependence/verify_diagnostics.swift because it causes
// an error during type checking with the current version of function type
// lifetime checking, preventing the SIL diagnostic checks that file tests from
// running.
//
// TODO: Add more diagnostic tests when implementing closure context
// dependencies, including SILOptimizer diagnostic tests such as the one this
// originated as.
func testIndirectClosureResult<T>(f: () -> CNE<T>) -> CNE<T> {
  // expected-error @-1{{a function with a ~Escapable result needs a parameter to depend on}}
  // expected-note  @-2{{'@_lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
  f()
}
