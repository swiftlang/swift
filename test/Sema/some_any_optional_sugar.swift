// RUN: %target-typecheck-verify-swift -swift-version 6

protocol P {}
struct S: P {}

func nonsugaredOpaqueReturn() -> (some P)? {
  return sugaredOpaqueReturn()
}
func sugaredOpaqueReturn() -> some P? {
  if Bool.random() {
    return S()
  } else {
    return nil as S?
  }
}

// `(some P)!` is not allowed, so neither should this be.
func sugaredIUOOpaqueReturn() -> some P! {  // expected-error {{using '!' is not allowed here}}
                                            // expected-note@-1 {{use '?' instead}}
  return S()
}

func nonsugaredAnyParam(p: (any P)?) { sugaredAnyParam(p: p) }
func sugaredAnyParam(p: any P?) { nonsugaredAnyParam(p: p) }

func nonsugaredSomeParam(p: (some P)?) {
  sugaredSomeParam(p: p)
  explicitGenericParam(p: p)
}
func sugaredSomeParam(p: some P?) { nonsugaredSomeParam(p: p) }
func explicitGenericParam<T: P>(p: T?) { nonsugaredSomeParam(p: p) }

func testSome(p: some P?) {
  let _: any P? = p
  if let val = p {
    let _: any P = val
  }
}

let x: any P? = S()
let y: some P? = S()

struct MyType {
  var sugaredField1: any P?
  var sugaredField2: any P!
  var sugaredField3: any P?!
  var sugaredField4: any P??
  var nonsugaredField1: (any P)?
  var nonsugaredField2: (any P)!
  var nonsugaredField3: (any P)?!
  var nonsugaredField4: (any P)?

  var sugaredField5: some P? { S() }
  var sugaredField6: some P?? { S() }
  var nonsugaredField5: (some P)? { S() }
  var nonsugaredField6: (some P)?? { S() }

  // IUOs are not allowed beneath the top-level, make sure that is still diagnosed.
  var sugaredField7: any P!?  // expected-error {{using '!' is not allowed here}}
                              // expected-note@-1 {{use '?' instead}}
  var nonsugaredField7: (any P)!?  // expected-error {{using '!' is not allowed here}}
                                   // expected-note@-1 {{use '?' instead}}

  // `(some P)!` and `(some P)?!` are not allowed, so neither should these be.
  var sugaredField8: some P! { S() }  // expected-error {{using '!' is not allowed here}}
                                      // expected-note@-1 {{use '?' instead}}
  var sugaredField9: some P?! { S() }  // expected-error {{using '!' is not allowed here}}
                                       // expected-note@-1 {{use '?' instead}}
}

// Deeper optionals
func testDoubleOptional(p: any P??) {
  let _: (any P)?? = p
}
func testDoubleOptionalSome() -> some P?? {
  testDoubleOptional(p: S())
  return S()
}

// Works in expression context, too
let _: [any P?] = [any P?]()
let _: [any P??] = [any P??]()

// Parentheses and compositions
protocol Q {}
extension S: Q {}

let _: any (P)? = S()
let _: any (P & Q)? = S()

func testParens(_: some (P)?) {}
testParens(S())

func testComposition(_: some (P & Q)?) {}
testComposition(S())

// Nested protocols
struct Outer { protocol InnerP {} }
func testNested(_: any Outer.InnerP?) {}

protocol QP {}
protocol RP {}
extension Optional {
  typealias P = QP
}
func f98(_: any RP?.P) {}
func f99(_: any RP?.P?) {}

// ...but these should still be errors:

// expected-error@+2 {{confusing use of optional after a 'some' or 'any' composition; use parentheses to clarify precedence}}{{12-12=(}}{{17-17=)}}
// expected-error@+1 {{non-protocol, non-class type '(any Q)?' cannot be used within a protocol-constrained type}}
let _: any P & Q? = S()

// expected-error@+3 {{confusing use of optional after a 'some' or 'any' composition; use parentheses to clarify precedence}}{{12-12=(}}{{18-18=)}}
// expected-error@+2 {{non-protocol, non-class type '(any P)?' cannot be used within a protocol-constrained type}}
// expected-error@+1 {{non-protocol, non-class type '(any Q)?' cannot be used within a protocol-constrained type}}
let _: any P? & Q? = S()

// expected-error@+1 {{non-protocol, non-class type '(any P)?' cannot be used within a protocol-constrained type}}
let _: any P? & Q = S()
