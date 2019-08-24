// RUN: %target-run-simple-swift 2>&1 | %FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest


_setTestSuiteFailedCallback() { print("hooray") }

//
// Test that harness aborts when no tests are run.
//

var ForgotToRunTestsTestSuite = TestSuite("ForgotToRunTests")

ForgotToRunTestsTestSuite.test("Don'tRunThis") {
  expectUnreachable()
}

// runAllTests() deliberately not called.
// runNoTests() deliberately not called.

// CHECK: Ran no tests
// CHECK: hooray
