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
