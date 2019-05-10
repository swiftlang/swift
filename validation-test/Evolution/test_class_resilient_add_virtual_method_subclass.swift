// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import class_resilient_add_virtual_method_subclass


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

class AddVirtualMethodGenericSubclass<T> : AddVirtualMethod {
  func f3(_ t: T) -> [Int : T] {
     return [f1() : t]
  }
}

ClassAddVirtualMethodSubclassTest.test("AddVirtualMethodGeneric") {
  let t = AddVirtualMethodGenericSubclass<String>()

  expectEqual(1, t.f1())
  expectEqual([1 : "hi"], t.f3("hi"))
}

runAllTests()
