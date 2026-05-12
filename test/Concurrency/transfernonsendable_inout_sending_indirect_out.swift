// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify %s -o /dev/null -parse-as-library

// This test verifies that the region isolation pass correctly detects when an
// 'inout sending' parameter is returned via an indirect @out result parameter
// (both normal and sending). This is distinct from the direct-return case tested
// in transfernonsendable_inout_sending_params.swift, because the return value
// flows through a copy_addr to an @out parameter rather than through a direct
// 'return' instruction in SIL.

// REQUIRES: concurrency

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {}

func useValue<T>(_ t: T) {}

///////////////////////////////////////////////////////
// MARK: Closures with inout sending indirect return //
///////////////////////////////////////////////////////

// A generic higher-order function whose closure takes an inout sending param.
// Because the result type T is generic, the closure's return is indirect (@out).
func takeClosure<T>(_ fn: (inout sending NonSendableKlass) -> T) -> T {
  fatalError()
}

func takeClosureThrowing<T>(_ fn: (inout sending NonSendableKlass) throws -> T) rethrows -> T {
  fatalError()
}

// The closure { $0 } directly returns the inout sending parameter via @out.
func testClosureReturnsInOutSendingParam() {
  let _ = takeClosure { $0 } // expected-error {{'inout sending' parameter '$0' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter '$0' risks concurrent access as caller assumes '$0' and result can be sent to different isolation domains}}
}

// Same pattern but with an explicit closure body.
func testClosureExplicitReturn() {
  let _ = takeClosure { (x: inout sending NonSendableKlass) -> NonSendableKlass in
    return x // expected-error {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

// Returning a value derived from the inout sending parameter.
func testClosureReturnsDerivedValue() {
  let _ = takeClosure { (x: inout sending NonSendableKlass) -> NonSendableKlass in
    let y = x
    return y // expected-error {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

// Safe case: closure returns a fresh value (should NOT produce an error).
func testClosureReturnsFreshValue() {
  let _ = takeClosure { (x: inout sending NonSendableKlass) -> NonSendableKlass in
    x = NonSendableKlass()
    return NonSendableKlass()
  }
}

func testClosureAssignsOverButStillReturnsIt() {
  let _ = takeClosure { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    state = NonSendableKlass()
    return state // expected-error {{'inout sending' parameter 'state' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'state' risks concurrent access as caller assumes 'state' and result can be sent to different isolation domains}}
  }
}

func testClosureAssignsToCapture() {
  let m2 = NonSendableKlass()
  let _ = takeClosure { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    state = m2
    return state // expected-error {{'inout sending' parameter 'state' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'state' risks concurrent access as caller assumes 'state' and result can be sent to different isolation domains}}
  }
}

/////////////////////////////////////////////////////////
// MARK: Functions with inout sending indirect return  //
/////////////////////////////////////////////////////////

// A function returning 'some Any' has an indirect @out result in SIL.
func returnInOutSendingViaOpaqueResult(_ x: inout sending NonSendableKlass) -> some Any {
  return x // expected-error {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

// A generic function returning T also has an indirect @out result.
func returnInOutSendingGeneric<T: AnyObject>(_ x: inout sending T) -> T {
  return x // expected-error {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

// Returning a derived value through indirect out.
func returnDerivedViaOpaqueResult(_ x: inout sending NonSendableKlass) -> some Any {
  let y = x
  return y // expected-error {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

// Safe case: function returns a fresh value via opaque result (should NOT produce an error).
func returnFreshViaOpaqueResult(_ x: inout sending NonSendableKlass) -> some Any {
  x = NonSendableKlass()
  return NonSendableKlass()
}

/////////////////////////////////////////////////////////////////
// MARK: Closures with inout sending and sending indirect return //
/////////////////////////////////////////////////////////////////

// A generic higher-order function whose closure takes an inout sending param
// and returns a sending result. Because the result type T is generic, the
// closure's return is indirect (@out) AND marked sending.
func takeSendingClosure<T>(_ fn: (inout sending NonSendableKlass) -> sending T) -> T {
  fatalError()
}

func takeSendingClosureThrowing<T>(_ fn: (inout sending NonSendableKlass) throws -> sending T) rethrows -> T {
  fatalError()
}

// The closure { $0 } directly returns the inout sending parameter via sending @out.
func testSendingClosureReturnsInOutSendingParam() {
  let _ = takeSendingClosure { $0 } // expected-error {{'inout sending' parameter '$0' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter '$0' risks concurrent access as caller assumes '$0' and result can be sent to different isolation domains}}
}

// Same pattern but with an explicit closure body.
func testSendingClosureExplicitReturn() {
  let _ = takeSendingClosure { (x: inout sending NonSendableKlass) -> NonSendableKlass in
    return x // expected-error {{'inout sending' parameter 'x' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

// Returning a value derived from the inout sending parameter.
func testSendingClosureReturnsDerivedValue() {
  let _ = takeSendingClosure { (x: inout sending NonSendableKlass) -> NonSendableKlass in
    let y = x
    return y // expected-error {{'y' cannot be returned}}
    // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
  }
}

// Safe case: closure returns a fresh value (should NOT produce an error).
func testSendingClosureReturnsFreshValue() {
  let _ = takeSendingClosure { (x: inout sending NonSendableKlass) -> NonSendableKlass in
    x = NonSendableKlass()
    return NonSendableKlass()
  }
}

func testSendingClosureAssignsOverButStillReturnsIt() {
  let _ = takeSendingClosure { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    state = NonSendableKlass()
    return state // expected-error {{'inout sending' parameter 'state' cannot be returned}}
    // expected-note @-1 {{returning 'inout sending' parameter 'state' risks concurrent access as caller assumes 'state' and result can be sent to different isolation domains}}
  }
}

func testSendingClosureAssignsToCapture() {
  let m2 = NonSendableKlass()
  let _ = takeSendingClosure { (state: inout sending NonSendableKlass) -> NonSendableKlass in
    state = m2
    return state // expected-error {{returning task-isolated 'state' as a 'sending' result risks causing data races}}
    // expected-note @-1 {{returning task-isolated 'state' risks causing data races since the caller assumes that 'state' can be safely sent to other isolation domains}}
    // expected-error @-2 {{'inout sending' parameter 'state' cannot be task-isolated at end of function}}
    // expected-note @-3 {{task-isolated 'state' risks causing races in between task-isolated uses and caller uses since caller assumes value is not actor isolated}}
  }
}

///////////////////////////////////////////////////////////////
// MARK: Functions with inout sending and sending indirect return //
///////////////////////////////////////////////////////////////

// A function returning 'sending some Any' has an indirect @out sending result in SIL.
func returnInOutSendingViaSendingOpaqueResult(_ x: inout sending NonSendableKlass) -> sending some Any {
  return x // expected-error {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

// A generic function returning sending T also has an indirect @out sending result.
func returnInOutSendingGenericSending<T: AnyObject>(_ x: inout sending T) -> sending T {
  return x // expected-error {{'inout sending' parameter 'x' cannot be returned}}
  // expected-note @-1 {{returning 'inout sending' parameter 'x' risks concurrent access as caller assumes 'x' and result can be sent to different isolation domains}}
}

// Returning a derived value through sending indirect out.
func returnDerivedViaSendingOpaqueResult(_ x: inout sending NonSendableKlass) -> sending some Any {
  let y = x
  return y // expected-error {{'y' cannot be returned}}
  // expected-note @-1 {{returning 'y' risks concurrent access to 'inout sending' parameter 'x' as caller assumes 'x' and result can be sent to different isolation domains}}
}

// Safe case: function returns a fresh value via sending opaque result (should NOT produce an error).
func returnFreshViaSendingOpaqueResult(_ x: inout sending NonSendableKlass) -> sending some Any {
  x = NonSendableKlass()
  return NonSendableKlass()
}
