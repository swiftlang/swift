// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_change_size.swift -o %t/before/struct_change_size.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/struct_change_size.swift -o %t/before/struct_change_size.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_change_size.swift -o %t/after/struct_change_size.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/struct_change_size.swift -o %t/after/struct_change_size.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/struct_change_size.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/struct_change_size.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/struct_change_size.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/struct_change_size.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import struct_change_size

var StructChangeSizeTest = TestSuite("StructChangeSize")

StructChangeSizeTest.test("ChangeSize") {
  var t = ChangeSize()

  do {
    expectEqual(t.value, 0)
    t.value = 101
    expectEqual(t.value, 101)
  }
}

StructChangeSizeTest.test("ChangeFieldOffsetsOfFixedLayout") {
  var t = ChangeFieldOffsetsOfFixedLayout()

  do {
    expectEqual(t.getTotal(), 0)
  }

  do {
    t.v1.value = 12
    expectEqual(t.getTotal(), 12)
    t.v2.value = -24
    expectEqual(t.getTotal(), -12)
  }
}

struct ChangeFieldOffsetsOfMyFixedLayout {
  var v1: ChangeSize = ChangeSize()
  var v2: ChangeSize = ChangeSize()

  func getTotal() -> Int32 {
    return v1.value + v2.value
  }
}

StructChangeSizeTest.test("ChangeFieldOffsetsOfMyFixedLayout") {
  var t = ChangeFieldOffsetsOfMyFixedLayout()

  do {
    expectEqual(t.getTotal(), 0)
  }

  do {
    t.v1.value = 12
    expectEqual(t.getTotal(), 12)
    t.v2.value = -24
    expectEqual(t.getTotal(), -12)
  }
}

runAllTests()

