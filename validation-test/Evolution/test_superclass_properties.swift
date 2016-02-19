// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import superclass_properties

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
      expectEqual(Leaf.classProperty, "Base.classProperty")
    } else {
      expectEqual(Leaf().property, "InBetween.property")
      expectEqual(Leaf.classProperty, "InBetween.classProperty")
    }
  }
}

SuperclassPropertiesTest.test("ChangeRoot") {
  do {
    class Leaf : ChangeRoot {
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
      expectEqual(Leaf().property, "OtherBase.property")
      expectEqual(Leaf.classProperty, "OtherBase.classProperty")
    }
  }
}

runAllTests()
