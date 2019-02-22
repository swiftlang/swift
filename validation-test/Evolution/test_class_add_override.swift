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

  g.count = 41
  if getVersion() == 0 {
    expectEqual(41, g.count)
  } else {
    expectEqual(40, g.count)
  }

  let kp = \AddOverrideGeneric<Int>.count

  g[keyPath: kp] = 51
  if getVersion() == 0 {
    expectEqual(51, g[keyPath: kp])
  } else {
    expectEqual(50, g[keyPath: kp])
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

  g.count = 41
  if getVersion() == 0 {
    expectEqual(41, g.count)
  } else {
    expectEqual(40, g.count)
  }

  let kp = \AddOverrideGenericSubclass<Int>.count

  g[keyPath: kp] = 51
  if getVersion() == 0 {
    expectEqual(51, g[keyPath: kp])
  } else {
    expectEqual(50, g[keyPath: kp])
  }
}

ClassAddOverrideTest.test("AddOverrideConcrete") {
  let c = AddOverrideConcrete()

  if getVersion() == 0 {
    expectEqual("Base", c.description)
  } else {
    expectEqual("Concrete", c.description)
  }

  c.count = 41
  if getVersion() == 0 {
    expectEqual(41, c.count)
  } else {
    expectEqual(40, c.count)
  }

  let kp = \AddOverrideConcrete.count

  c[keyPath: kp] = 51
  if getVersion() == 0 {
    expectEqual(51, c[keyPath: kp])
  } else {
    expectEqual(50, c[keyPath: kp])
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

  c.count = 41
  if getVersion() == 0 {
    expectEqual(41, c.count)
  } else {
    expectEqual(40, c.count)
  }

  let kp = \AddOverrideConcreteSubclass.count

  c[keyPath: kp] = 51
  if getVersion() == 0 {
    expectEqual(51, c[keyPath: kp])
  } else {
    expectEqual(50, c[keyPath: kp])
  }
}

runAllTests()
