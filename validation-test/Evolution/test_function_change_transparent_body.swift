// RUN: %target-resilience-test-wmo
// REQUIRES: executable_test

// FIXME: shouldn't need -whole-module-optimization here; we need to fix the
// frontend to merge serialized SIL functions from different translation units

import StdlibUnittest
import function_change_transparent_body

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var ChangeTransparentBodyTest = TestSuite("ChangeTransparentBody")

ChangeTransparentBodyTest.test("ChangeTransparentBody") {

#if BEFORE
  expectEqual(getBuildVersion(), 0)
#else
  expectEqual(getBuildVersion(), 1)
#endif

}

runAllTests()
