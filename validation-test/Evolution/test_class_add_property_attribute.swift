// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import class_add_property_attribute


var AddPropertyAttribute = TestSuite("AddPropertyAttribute")

AddPropertyAttribute.test("AddPropertyAttribute") {
  do {
    let x = MathClass()
    let y = MathClass()

    let a = Attributed(x: x, y: y)
    expectTrue(x === a.x)
    expectTrue(y === a.y)
  }
}

runAllTests()
