// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let PrintTests = TestSuite("PrintBoolean")

PrintTests.test("CustomStringConvertible") {
  func hasDescription(any: Any) {
    expectTrue(any is CustomStringConvertible)
  }

  hasDescription(Bool(true))
  hasDescription(CBool(true))
}

PrintTests.test("Printable") {
  expectPrinted("true", CBool(true))
  expectPrinted("false", CBool(false))
  
  expectPrinted("true", Bool(true))
  expectPrinted("false", Bool(false))
  
  expectPrinted("true", true)
  expectPrinted("false", false)
}

runAllTests()
