// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var CrashTest = TestSuite("CrashTest")

CrashTest.test("expectCrashLater(withMessage:)") {
  var i: Int? = nil
  expectCrashLater(withMessage: "Unexpectedly found nil while unwrapping an Optional value")
  _ = i!
  expectUnreachable()
  i = 0
}

runAllTests()
