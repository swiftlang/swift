// RUN: %target-build-swift -module-name a %s -o %t.out
// RUN: %target-run %t.out
// REQUIRES: executable_test
// XFAIL: *

//
// Check that failures coming from StdlibUnittest are counted as failures by lit.
//

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var TestSuiteFails = TestSuite("TestSuiteFails")

TestSuiteFails.test("fails") {
  expectEqual(1, 2)
}

runAllTests()

