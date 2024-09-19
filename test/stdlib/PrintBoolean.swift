// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


let PrintTests = TestSuite("PrintBoolean")

PrintTests.test("CustomStringConvertible") {
  func hasDescription(_ any: Any) {
    expectTrue(any is CustomStringConvertible)
  }

  hasDescription(true as Bool)
  hasDescription(true as CBool)
}

PrintTests.test("Printable") {
  expectPrinted("true", true as CBool)
  expectPrinted("false", false as CBool)

  expectPrinted("true", true as Bool)
  expectPrinted("false", false as Bool)

  expectPrinted("true", true)
  expectPrinted("false", false)
}

PrintTests.test("LosslessStringConvertible") {
  checkLosslessStringConvertible([ true, false ])
}

runAllTests()
