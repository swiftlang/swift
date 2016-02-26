// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_change_stored_to_computed

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var ChangeStoredToComputedTest = TestSuite("ChangeStoredToComputed")

ChangeStoredToComputedTest.test("ChangeStoredToComputed") {
  var t = ChangeStoredToComputed()

  do {
    expectEqual(t.celsius, 0)
    expectEqual(t.fahrenheit, 32)
  }

  do {
    t.celsius = 10
    expectEqual(t.celsius, 10)
    expectEqual(t.fahrenheit, 50)
  }

  do {
    func increaseTemperature(inout t: Int) {
      t += 10
    }

    increaseTemperature(&t.celsius)

    expectEqual(t.celsius, 20)
    expectEqual(t.fahrenheit, 68)
  }
}

runAllTests()
