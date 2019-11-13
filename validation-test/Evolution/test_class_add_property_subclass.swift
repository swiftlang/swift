// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_add_property_subclass


var ClassAddPropertySubclassTest = TestSuite("ClassAddPropertySubclass")

class AddStoredPropertySubclass : AddStoredProperty {
  var z: Int

  init(x: Int, z: Int) {
    self.z = z
    super.init(x: x)
  }
}

ClassAddPropertySubclassTest.test("AddStoredProperty") {
  let t = AddStoredPropertySubclass(x: 1, z: 2)

  expectEqual(1, t.get().0)
  expectEqual(-1, t.get().1)
  expectEqual(2, t.z)
}

class AddStoredPropertyGenericSubclass<T> : AddStoredProperty {
  var z: T

  init(x: Int, z: T) {
    self.z = z
    super.init(x: x)
  }
}

ClassAddPropertySubclassTest.test("AddStoredPropertyGeneric") {
  let t = AddStoredPropertyGenericSubclass(x: 1, z: "hello world")

  expectEqual(1, t.get().0)
  expectEqual(-1, t.get().1)
  expectEqual("hello world", t.z)
}

runAllTests()
