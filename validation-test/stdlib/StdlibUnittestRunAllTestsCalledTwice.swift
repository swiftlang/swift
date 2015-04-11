// RUN: %target-run-simple-swift 2>&1 | FileCheck %s

import StdlibUnittest

_setTestSuiteFailedCallback() { println("abort()") }

//
// Check that calling runAllTests() twice is an error.
//

var TestSuitePasses = TestSuite("TestSuitePasses")

TestSuitePasses.test("passes") {
  expectEqual(1, 1)
}

runAllTests()
runAllTests()
// CHECK: runAllTests() called twice.  It is not allowed, aborting.
// CHECK: abort()

