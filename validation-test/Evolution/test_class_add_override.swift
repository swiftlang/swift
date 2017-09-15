// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_add_override


var ClassAddOverrideTest = TestSuite("ClassAddOverride")

ClassAddOverrideTest.test("AddOverrideGeneric") {
  let g = AddOverrideGeneric<Int>()

  if getVersion() == 0 {
    expectEqual("Base", g.description)
  } else {
    expectEqual("Generic", g.description)
  }
}

class AddOverrideGenericSubclass<T> : AddOverrideGeneric<T> {
  func meaningOfLife() -> Int { return 42 }
}

ClassAddOverrideTest.test("AddOverrideGenericSubclass") {
  let g = AddOverrideGenericSubclass<Int>()

  expectEqual(42, g.meaningOfLife())

  if getVersion() == 0 {
    expectEqual("Base", g.description)
  } else {
    expectEqual("Generic", g.description)
  }
}

ClassAddOverrideTest.test("AddOverrideConcrete") {
  let c = AddOverrideConcrete()

  if getVersion() == 0 {
    expectEqual("Base", c.description)
  } else {
    expectEqual("Concrete", c.description)
  }
}

class AddOverrideConcreteSubclass : AddOverrideConcrete {
  func meaningOfLife() -> Int { return 42 }
}

ClassAddOverrideTest.test("AddOverrideConcreteSubclass") {
  let c = AddOverrideConcreteSubclass()

  expectEqual(42, c.meaningOfLife())

  if getVersion() == 0 {
    expectEqual("Base", c.description)
  } else {
    expectEqual("Concrete", c.description)
  }
}

runAllTests()
