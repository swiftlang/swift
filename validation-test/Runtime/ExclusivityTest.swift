// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import RuntimeUnittest

func hasBackdeployedConcurrencyRuntime() -> Bool {
  // If the stdlib we've loaded predates Swift 5.5, then we're running on a back
  // deployed concurrency runtime, which has the side effect of disabling
  // regular runtime exclusivity checks.
  if #available(SwiftStdlib 5.5, *) { return false } // recent enough production stdlib
  if #available(SwiftStdlib 9999, *) { return false } // dev stdlib
  return true
}

var ExclusivityTestSuite = TestSuite("Exclusivity")

ExclusivityTestSuite.test("testExclusivityNullPC")
  .skip(.custom(
    { hasBackdeployedConcurrencyRuntime() },
    reason: "the back deployed concurrency runtime doesn't do exclusivity checks"))
  .code {
  expectCrash(withMessage: "Simultaneous accesses") {
      SwiftRuntimeUnitTest.testExclusivityNullPC()
  }
}

ExclusivityTestSuite.test("testExclusivityPCOne")
  .skip(.custom(
    { hasBackdeployedConcurrencyRuntime() },
    reason: "the back deployed concurrency runtime doesn't do exclusivity checks"))
  .code {
  expectCrash(withMessage: "Simultaneous accesses") {
    SwiftRuntimeUnitTest.testExclusivityPCOne()
  }
}

ExclusivityTestSuite.test("testExclusivityBogusPC")
  .skip(.custom(
    { hasBackdeployedConcurrencyRuntime() },
    reason: "the back deployed concurrency runtime doesn't do exclusivity checks"))
  .code {
  expectCrash(withMessage: "Simultaneous accesses") {
    SwiftRuntimeUnitTest.testExclusivityBogusPC()
  }
}

ExclusivityTestSuite.test("testExclusivityNonNestedPC")
  .skip(.custom(
    { hasBackdeployedConcurrencyRuntime() },
    reason: "the back deployed concurrency runtime doesn't do exclusivity checks"))
  .code {
  SwiftRuntimeUnitTest.testExclusivityNonNestedPC()
}

runAllTests()
