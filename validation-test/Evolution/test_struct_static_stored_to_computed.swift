// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_static_stored_to_computed

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var StructStaticChangeStoredToComputedTest = TestSuite("StructStaticChangeStoredToComputed")

StructStaticChangeStoredToComputedTest.test("ChangeStoredToComputed") {
  do {
    @inline(never) func twice(inout x: Int) {
      x *= 2
    }

    expectEqual(ChangeStoredToComputed.value, 0)
    ChangeStoredToComputed.value = 32
    expectEqual(ChangeStoredToComputed.value, 32)
    ChangeStoredToComputed.value = -128
    expectEqual(ChangeStoredToComputed.value, -128)
    twice(&ChangeStoredToComputed.value)
    expectEqual(ChangeStoredToComputed.value, -256)
  }
}

runAllTests()
