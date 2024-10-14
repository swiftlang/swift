// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature SendingArgsAndResults -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: asserts

class NonSendable {
  init(){}
}

func testArg(_ x: sending NonSendable) {
}

func testResult() -> sending NonSendable {
  return NonSendable()
}

func testArgResult(_ x: sending NonSendable) -> sending NonSendable {
}

func testVarDeclDoesntWork() {
  var x: sending NonSendable // expected-error {{'sending' may only be used on parameter}}
}

func testVarDeclTupleElt() -> (sending NonSendable, NonSendable) {} // expected-error {{'sending' cannot be applied to tuple elements}}

func testVarDeclTuple2(_ x: (sending NonSendable)) {}
func testVarDeclTuple2(_ x: (sending NonSendable, NonSendable)) {} // expected-error {{'sending' cannot be applied to tuple elements}}

func testArgWithConsumingWrongOrder(_ x: sending consuming NonSendable, _ y: sending inout NonSendable) {}
// expected-error @-1 {{'sending' must be placed after specifier 'consuming'}}
// expected-error @-2 {{'sending' must be placed after specifier 'inout'}}

func testArgWithConsumingWrongOrderType(_ x: (sending consuming NonSendable, sending inout NonSendable) -> ()) {}
// expected-error @-1 {{'sending' must be placed after specifier 'consuming'}}
// expected-error @-2 {{'sending' must be placed after specifier 'inout'}}

func testBorrowSending(_ x: borrowing sending NonSendable) {}
// expected-error @-1 {{'sending' cannot be used together with 'borrowing'}}
