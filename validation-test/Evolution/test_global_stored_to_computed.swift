// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import global_stored_to_computed

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var GlobalStoredToComputed = TestSuite("GlobalStoredToComputed")

GlobalStoredToComputed.test("ChangeStoredToComputed") {
  do {
    @inline(never) func increment(inout x: Int) {
      x += 1
    }

    expectEqual(globalStoredToComputed, 0)
    increment(&globalStoredToComputed)
    expectEqual(globalStoredToComputed, 1)
    globalStoredToComputed = 0xbadf00d
    expectEqual(globalStoredToComputed, 0xbadf00d)
  }
}

runAllTests()

