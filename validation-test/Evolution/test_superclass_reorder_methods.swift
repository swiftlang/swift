// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import superclass_reorder_methods


var SuperclassReorderMethodsTest = TestSuite("SuperclassReorderMethods")

SuperclassReorderMethodsTest.test("TestOverrides") {
  class MyDerived : Base {
    override func firstMethod() -> Int {
      return 3
    }
    override func secondMethod() -> Int {
      return 4
    }
  }

  expectEqual(MyDerived().callOverriddenMethods(), 34)
}

SuperclassReorderMethodsTest.test("TestSuper") {
  class MyDerived : Base {
    override func firstMethod() -> Int {
      return super.firstMethod() + 3
    }
    override func secondMethod() -> Int {
      return super.secondMethod() + 3
    }
  }

  expectEqual(MyDerived().callOverriddenMethods(), 45)
}

extension Derived {
  public func firstMethodExt() -> Int {
    return firstMethod() + super.firstMethod()
  }

  public func secondMethodExt() -> Int {
    return secondMethod() + super.secondMethod()
  }
}

SuperclassReorderMethodsTest.test("TestSuperExtension") {
  let obj = Derived()
  expectEqual(obj.firstMethodExt(), 11)
  expectEqual(obj.secondMethodExt(), 22)
}

runAllTests()

