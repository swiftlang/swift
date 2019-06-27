// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_change_stored_to_computed


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
    func increaseTemperature(_ t: inout Int) {
      t += 10
    }

    increaseTemperature(&t.celsius)

    expectEqual(t.celsius, 20)
    expectEqual(t.fahrenheit, 68)
  }
}

runAllTests()
