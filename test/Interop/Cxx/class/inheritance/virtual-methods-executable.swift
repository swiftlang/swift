// RUN: %target-run-simple-swift(-I %S/Inputs -cxx-interoperability-mode=default)
// RUN: %target-run-simple-swift(-g -I %S/Inputs -cxx-interoperability-mode=default)

// REQUIRES: executable_test

import StdlibUnittest
import VirtualMethods

var VirtualMethodsTestSuite = TestSuite("Virtual Methods")

VirtualMethodsTestSuite.test("value type") {
  var d2 = Derived2()
  expectEqual(999, d2.f())

  var d3 = Derived3()
  expectEqual(222, d3.f())

  var d4 = Derived4()
  expectEqual(111, d4.f())

  let d5 = DerivedFromCallsPureMethod()
  expectEqual(790, d5.getInt())
  expectEqual(789, d5.getPureInt())

  let d6 = DerivedFromDerivedFromCallsPureMethod()
  expectEqual(790, d6.getInt())
  expectEqual(789, d6.getPureInt())
}

VirtualMethodsTestSuite.test("renamed virtual methods") {
  let vrb = VirtualRenamedBase()
  expectEqual(101, vrb.swiftName())
  let vri = VirtualRenamedInherited()
  expectEqual(101, vri.swiftName())
  let vro = VirtualRenamedOverridden()
  expectEqual(303, vro.swiftName())
  let pvro = PureVirtualRenamedOverridden()
  expectEqual(404, pvro.swiftName())
}

runAllTests()
