// RUN: %target-resilience-test --no-symbol-diff
// REQUIRES: executable_test

import StdlibUnittest
import superclass_properties


var SuperclassPropertiesTest = TestSuite("SuperclassProperties")

SuperclassPropertiesTest.test("AddInterposingProperty") {
  do {
    class Leaf : AddInterposingProperty {
      override var property: String {
        return super.property
      }
      override class var classProperty: String {
        return super.classProperty
      }
    }
    if getVersion() == 0 {
      expectEqual(Leaf().property, "Base.property")
      expectEqual(Leaf.classProperty, "Base.classProperty")
    } else {
      expectEqual(Leaf().property, "AddInterposingProperty.property")
      expectEqual(Leaf.classProperty, "AddInterposingProperty.classProperty")
    }
  }
}

SuperclassPropertiesTest.test("RemoveInterposingProperty") {
  do {
    class Leaf : RemoveInterposingProperty {
      override var property: String {
        return super.property
      }
      override class var classProperty: String {
        return super.classProperty
      }
    }
    if getVersion() == 0 {
      expectEqual(Leaf().property, "RemoveInterposingProperty.property")
      expectEqual(Leaf.classProperty, "RemoveInterposingProperty.classProperty")
    } else {
      expectEqual(Leaf().property, "Base.property")
      expectEqual(Leaf.classProperty, "Base.classProperty")
    }
  }
}

SuperclassPropertiesTest.test("InsertSuperclass") {
  do {
    class Leaf : InsertSuperclass {
      override var property: String {
        return super.property
      }
      override class var classProperty: String {
        return super.classProperty
      }
    }
    if getVersion() == 0 {
      expectEqual(Leaf().property, "Base.property")
      expectEqual(Leaf().nonOverriddenProperty, "Base.nonOverriddenProperty")
      expectEqual(Leaf.classProperty, "Base.classProperty")
    } else {
      expectEqual(Leaf().property, "InBetween.property")
      expectEqual(Leaf().nonOverriddenProperty, "Base.nonOverriddenProperty")
      expectEqual(Leaf.classProperty, "InBetween.classProperty")
    }
  }
}

runAllTests()
