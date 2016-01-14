// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_add_remove_properties.swift -o %t/before/struct_add_remove_properties.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_add_remove_properties.swift -o %t/before/struct_add_remove_properties.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_add_remove_properties.swift -o %t/after/struct_add_remove_properties.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_add_remove_properties.swift -o %t/after/struct_add_remove_properties.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/struct_add_remove_properties.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/struct_add_remove_properties.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/struct_add_remove_properties.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/struct_add_remove_properties.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import struct_add_remove_properties

var StructAddRemovePropertiesTest = TestSuite("StructAddRemoveProperties")

StructAddRemovePropertiesTest.test("ChangeStoredToComputed") {
  var t = ChangeStoredToComputed()

  do {
    expectEqual(t.celsius, 0)
    expectEqual(t.fahrenheit, 32)
  }

  do {
    t.celsius = 10
    expectEqual(t.celsius, 10)
    expectEqual(t.fahrenheit, 50)
  }

  do {
    func increaseTemperature(inout t: Int) {
      t += 10
    }

    increaseTemperature(&t.celsius)

    expectEqual(t.celsius, 20)
    expectEqual(t.fahrenheit, 68)
  }
}

StructAddRemovePropertiesTest.test("ChangeEmptyToNonEmpty") {
  var t = ChangeEmptyToNonEmpty()

  do {
    expectEqual(t.property, 0)
    t.property += 1
    expectEqual(t.property, 1)
  }

  do {
    func increment(inout t: Int) {
      t += 1
    }

    increment(&t.property)
    expectEqual(t.property, 2)
  }

  do {
    expectEqual(getProperty(t), 2)
  }
}

StructAddRemovePropertiesTest.test("AddStoredProperty") {
  var t = AddStoredProperty()

  do {
    expectEqual(t.count, 0)
    t.count = 100
    expectEqual(t.count, 100)
  }

  do {
    t.count = -1000
    expectEqual(t.count, -1000)
  }
}

runAllTests()
