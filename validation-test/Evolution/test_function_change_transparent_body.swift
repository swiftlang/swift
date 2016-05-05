// RUN: %target-resilience-test-wmo
// REQUIRES: executable_test

// FIXME: shouldn't need -whole-module-optimization here; we need to fix the
// frontend to merge serialized SIL functions from different translation units

import StdlibUnittest
import function_change_transparent_body


var ChangeTransparentBodyTest = TestSuite("ChangeTransparentBody")

ChangeTransparentBodyTest.test("ChangeTransparentBody") {

#if BEFORE
  expectEqual(0, getBuildVersion())
#else
  expectEqual(1, getBuildVersion())
#endif

}

ChangeTransparentBodyTest.test("ChangeNonTransparentClosure") {

  if getVersion() == 0 {
    expectEqual(202, getFunction(2)(101))
  } else {
    expectEqual(101, getFunction(2)(101))
  }

}

ChangeTransparentBodyTest.test("ChangeTransparentClosure") {

#if BEFORE
  expectEqual(202, getTransparentFunction(2)(101))
#else
  expectEqual(101, getTransparentFunction(2)(101))
#endif

}

runAllTests()
