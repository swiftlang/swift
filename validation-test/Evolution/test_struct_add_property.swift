// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_add_property.swift -o %t/before/struct_add_property.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_add_property.swift -o %t/before/struct_add_property.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_add_property.swift -o %t/after/struct_add_property.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_add_property.swift -o %t/after/struct_add_property.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/struct_add_property.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/struct_add_property.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/struct_add_property.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/struct_add_property.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

// REQUIRES: executable_test

import StdlibUnittest
import struct_add_property

var StructAddPropertyTest = TestSuite("StructAddProperty")

StructAddPropertyTest.test("AddStoredProperty") {
  var t1 = AddStoredProperty()
  var t2 = AddStoredProperty()

  do {
    expectEqual(t1.forth, "Chuck Moore")
    expectEqual(t2.forth, "Chuck Moore")
  }

  do {
    t1.forth = "Elizabeth Rather"
    expectEqual(t1.forth, "Elizabeth Rather")
    expectEqual(t2.forth, "Chuck Moore")
  }


  do {
    if getVersion() > 0 {
      expectEqual(t1.languageDesigners, ["Elizabeth Rather",
                                         "John McCarthy",
                                         "Dennis Ritchie"])
      expectEqual(t2.languageDesigners, ["Chuck Moore",
                                         "John McCarthy",
                                         "Dennis Ritchie"])
    } else {
      expectEqual(t1.languageDesigners, ["Elizabeth Rather"])
      expectEqual(t2.languageDesigners, ["Chuck Moore"])
    }
  }
}

StructAddPropertyTest.test("ChangeEmptyToNonEmpty") {
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

runAllTests()
