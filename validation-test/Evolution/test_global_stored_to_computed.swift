// RUN: %target-resilience-test
// REQUIRES: executable_test

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

