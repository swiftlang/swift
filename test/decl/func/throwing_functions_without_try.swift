// RUN: %target-parse-verify-swift -enable-throw-without-try

// Test the -enable-throw-without-try option. Throwing function calls should
// not require annotation with 'try'.

func foo() throws -> Int { return 0 }

func baz() throws -> Int {
  var x: Int = 0
  x = foo() // no error
  x = try foo() // still no error
  return x
}

func baz2() -> Int {
  var x: Int = 0
  x = foo() // expected-error{{call can throw, but it is not marked with 'try' and the error is not handled}}
  x = try foo() // expected-error{{errors thrown from here are not handled}}
  return x
}
