// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

//
// Test that harness runs no tests and succeeds when runNoTests() is called.
//

var RunNoTestsTestSuite = TestSuite("RunNoTests")

RunNoTestsTestSuite.test("Don'tRunThis") {
  expectUnreachable()
}

runNoTests()
