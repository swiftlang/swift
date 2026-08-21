// RUN: %target-run-stdlib-swift(-O -assert-config Debug)
// RUN: %target-run-stdlib-swift(-O -assert-config Release)
// RUN: %target-run-stdlib-swift(-Onone -assert-config Debug)
// RUN: %target-run-stdlib-swift(-Onone -assert-config Release)

// REQUIRES: executable_test

import StdlibUnittest

var suite = TestSuite("issue 85382 tests")

suite.test("reproducer")
.require(.crashTesting)
.code {
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  assertionFailure("Should only crash in debug mode.")
}

suite.test("counter-example")
.require(.crashTesting)
.code {
  if _isDebugAssertConfiguration() {
    expectCrashLater()
  }
  assert(false, "Should only crash in debug mode.")
}

runAllTests()
