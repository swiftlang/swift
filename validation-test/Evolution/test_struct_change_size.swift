// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_change_size

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

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
