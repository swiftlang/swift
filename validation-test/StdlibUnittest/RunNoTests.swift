// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


//
// Test that harness runs no tests and succeeds when runNoTests() is called.
//

var RunNoTestsTestSuite = TestSuite("RunNoTests")

RunNoTestsTestSuite.test("Don'tRunThis") {
  expectUnreachable()
}

runNoTests()
