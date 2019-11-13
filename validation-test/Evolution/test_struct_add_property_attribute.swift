// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_add_property_attribute


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
