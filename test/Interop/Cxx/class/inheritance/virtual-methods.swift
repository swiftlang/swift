// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-5.9)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=swift-6)
// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=upcoming-swift)
// RUN: %target-run-simple-swift(-g -I %S/Inputs -cxx-interoperability-mode=default)

// REQUIRES: executable_test

import StdlibUnittest
import VirtualMethods

var VirtualMethodsTestSuite = TestSuite("Virtual Methods")

VirtualMethodsTestSuite.test("value type") {
  var d2 = Derived2()
  expectEqual(42, d2.f())

  var d3 = Derived3()
  expectEqual(42, d3.f())

  var d4 = Derived4()
  expectEqual(24, d4.f())

  let d5 = DerivedFromCallsPureMethod()
  expectEqual(790, d5.getInt())
  expectEqual(789, d5.getPureInt())

  let d6 = DerivedFromDerivedFromCallsPureMethod()
  expectEqual(790, d6.getInt())
  expectEqual(789, d6.getPureInt())
}

runAllTests()
