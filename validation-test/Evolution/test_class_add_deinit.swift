// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_add_deinit


var ClassAddDeinitTest = TestSuite("ClassAddDeinit")

ClassAddDeinitTest.test("Base") {
  do {
    let _ = Base()
  }
  expectEqual(getVersion(), count)
  count = 0
}

ClassAddDeinitTest.test("Derived") {
  do {
    let _ = Derived()
  }
  expectEqual(getVersion() + 10, count)
  count = 0
}

class Subclass : Derived {}

ClassAddDeinitTest.test("Subclass") {
  do {
    let _ = Subclass()
  }
  expectEqual(getVersion() + 10, count)
  count = 0
}


runAllTests()
