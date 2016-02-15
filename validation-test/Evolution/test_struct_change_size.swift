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

// REQUIRES: executable_test

import StdlibUnittest
import struct_change_size

var ClassChangeSizeTest = TestSuite("ClassChangeSize")

func increment(inout c: ChangeSize) {
  c.version += 1
}

ClassChangeSizeTest.test("ChangeFieldOffsetsOfFixedLayout") {
  var t = ChangeFieldOffsetsOfFixedLayout(major: 7, minor: 5, patch: 3)

  do {
    expectEqual(t.getVersion(), "7.5.3")
  }

  do {
    t.minor.version = 1
    t.patch.version = 2
    expectEqual(t.getVersion(), "7.1.2")
  }

  do {
    increment(&t.patch)
    expectEqual(t.getVersion(), "7.1.3")
  }
}

struct ChangeFieldOffsetsOfMyFixedLayout {
  init(major: Int32, minor: Int32, patch: Int32) {
    self.major = ChangeSize(version: major)
    self.minor = ChangeSize(version: minor)
    self.patch = ChangeSize(version: patch)
  }

  var major: ChangeSize
  var minor: ChangeSize
  var patch: ChangeSize

  func getVersion() -> String {
    return "\(major.version).\(minor.version).\(patch.version)"
  }
}

ClassChangeSizeTest.test("ChangeFieldOffsetsOfMyFixedLayout") {
  var t = ChangeFieldOffsetsOfMyFixedLayout(major: 9, minor: 2, patch: 1)

  do {
    expectEqual(t.getVersion(), "9.2.1")
  }

  do {
    t.major.version = 7
    t.minor.version = 6
    t.patch.version = 1
    expectEqual(t.getVersion(), "7.6.1")
  }

  do {
    increment(&t.patch)
    expectEqual(t.getVersion(), "7.6.2")
  }
}

runAllTests()
