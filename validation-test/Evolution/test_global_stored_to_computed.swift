// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import global_stored_to_computed


var GlobalStoredToComputed = TestSuite("GlobalStoredToComputed")

GlobalStoredToComputed.test("ChangeStoredToComputed") {
  do {
    @inline(never) func increment(_ x: inout Int) {
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

