// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/superclass_properties.swift -o %t/before/superclass_properties.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/superclass_properties.swift -o %t/before/superclass_properties.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/superclass_properties.swift -o %t/after/superclass_properties.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/superclass_properties.swift -o %t/after/superclass_properties.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/superclass_properties.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/superclass_properties.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/superclass_properties.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/superclass_properties.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

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
