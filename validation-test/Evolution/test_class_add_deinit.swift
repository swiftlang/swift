// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

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
