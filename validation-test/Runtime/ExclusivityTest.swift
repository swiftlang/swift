// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import RuntimeUnittest

var ExclusivityTestSuite = TestSuite("Exclusivity")

ExclusivityTestSuite.test("testExclusivityNullPC") {
  expectCrash(withMessage: "Simultaneous accesses") {
      SwiftRuntimeUnitTest.testExclusivityNullPC()
  }
}

ExclusivityTestSuite.test("testExclusivityPCOne") {
  expectCrash(withMessage: "Simultaneous accesses") {
    SwiftRuntimeUnitTest.testExclusivityPCOne()
  }
}

ExclusivityTestSuite.test("testExclusivityBogusPC") {
  expectCrash(withMessage: "Simultaneous accesses") {
    SwiftRuntimeUnitTest.testExclusivityBogusPC()
  }
}

ExclusivityTestSuite.test("testExclusivityNonNestedPC") {
  SwiftRuntimeUnitTest.testExclusivityNonNestedPC()
}

runAllTests()
