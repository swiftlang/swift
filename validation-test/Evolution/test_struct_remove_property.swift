// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_remove_property.swift -o %t/before/struct_remove_property.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_remove_property.swift -o %t/before/struct_remove_property.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_remove_property.swift -o %t/after/struct_remove_property.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_remove_property.swift -o %t/after/struct_remove_property.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/struct_remove_property.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/struct_remove_property.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/struct_remove_property.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/struct_remove_property.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

// REQUIRES: executable_test

import StdlibUnittest
import struct_remove_property

var StructRemovePropertyTest = TestSuite("StructRemoveProperty")

StructRemovePropertyTest.test("RemoveStoredProperty") {
  var dog = RemoveStoredProperty(first: "Barnaby",
                                 middle: "Winston",
                                 last: "Fluffington")

  do {
    expectEqual(dog.name, "Barnaby Winston Fluffington")
  }
}

runAllTests()
