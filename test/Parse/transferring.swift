// RUN: %target-typecheck-verify-swift -disable-availability-checking -enable-experimental-feature TransferringArgsAndResults

// REQUIRES: asserts

func testArg(_ x: transferring String) {
}

// Error only on results.
func testResult() -> transferring String {
  // expected-error @-1 {{'transferring' may only be used on parameter}}
  ""
}

// Error on the result.
func testArgResult(_ x: transferring String) -> transferring String {
  // expected-error @-1 {{'transferring' may only be used on parameter}}
}

func testVarDeclDoesntWork() {
  var x: transferring String // expected-error {{'transferring' may only be used on parameter}}
}

