// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import superclass_reorder_methods


var SuperclassReorderMethodsTest = TestSuite("SuperclassReorderMethods")

SuperclassReorderMethodsTest.test("TestOverrides") {
  do {
    class Derived : Base {
      override func firstMethod() -> Int {
        return 1
      }
      override func secondMethod() -> Int {
        return 2
      }
    }

    expectEqual(Derived().callOverriddenMethods(), 12)
  }
}

runAllTests()

