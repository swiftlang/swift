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

if #available(SwiftStdlib 5.8, *) {
  VirtualMethodsTestSuite.test("immortal reference type") {
    let i = Immortal.create()
    expectEqual(42, i.get42())
    expectEqual(0, i.getIntValue())

    let base = castToImmortalBase(i)
    expectEqual(42, base.get42())
    expectEqual(42, base.getOverridden42())
    expectEqual(0, base.getIntValue())

    i.setIntValue(566)
    expectEqual(566, i.getIntValue())
    expectEqual(566, base.getIntValue())

    let d = DerivedFromImmortal.create()
    expectEqual(42, d.get42())
    expectEqual(42, d.getOverridden42())
    d.setIntValue(321)
    expectEqual(321, d.getIntValue())
    let base2 = castToImmortalBase(castToImmortal(d))
    expectEqual(321, base2.getIntValue())
  }
}

#if !os(Windows) 
// FIXME in Windows, non-trivial C++ class with trivial ABI is not yet available in Swift
VirtualMethodsTestSuite.test("C++ virtual method with complex parameter") {
  @available(SwiftStdlib 5.8, *)
  func f(simpleClass: HasDestructor, immortalClass: Immortal2) {
    immortalClass.virtualMethod(simpleClass)
  }
}
#endif

runAllTests()
