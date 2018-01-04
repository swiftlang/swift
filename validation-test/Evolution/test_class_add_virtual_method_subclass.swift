// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_add_virtual_method_subclass


var ClassAddVirtualMethodSubclassTest = TestSuite("ClassAddVirtualMethodSubclass")

class AddVirtualMethodSubclass : AddVirtualMethod {
  func f3() -> Int {
    return f1() + 1
  }
}

ClassAddVirtualMethodSubclassTest.test("AddVirtualMethod") {
  let t = AddVirtualMethodSubclass()

  expectEqual(1, t.f1())
  expectEqual(2, t.f3())
}

class AddVirtualMethodGenericSubclass<T : Strideable> : AddVirtualMethod
    where T.Stride == Int {
  func f3(_ t: T) -> T {
    return t + f1()
  }
}

ClassAddVirtualMethodSubclassTest.test("AddVirtualMethodGeneric") {
  let t = AddVirtualMethodGenericSubclass<Int32>()

  expectEqual(1, t.f1())
  expectEqual(2, t.f3(1))
}

runAllTests()
