// RUN: %target-run-simple-swift 2>&1 | FileCheck %s
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

_setTestSuiteFailedCallback() { print("abort()") }

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

