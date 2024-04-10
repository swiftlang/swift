// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature TransferringArgsAndResults -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: asserts

func testArg(_ x: transferring String) {
}

func testResult() -> transferring String {
  ""
}

func testArgResult(_ x: transferring String) -> transferring String {
}

func testVarDeclDoesntWork() {
  var x: transferring String // expected-error {{'transferring' may only be used on parameter}}
}

func testVarDeclTupleElt() -> (transferring String, String) {} // expected-error {{'transferring' cannot be applied to tuple elements}}

func testVarDeclTuple2(_ x: (transferring String)) {}
func testVarDeclTuple2(_ x: (transferring String, String)) {} // expected-error {{'transferring' cannot be applied to tuple elements}}
