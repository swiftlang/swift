// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import global_change_size

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
import SwiftPrivatePthreadExtras
#if _runtime(_ObjC)
import ObjectiveC
#endif

var GlobalChangeSizeTest = TestSuite("GlobalChangeSize")

var globalChangeEmptyToNonEmpty = ChangeEmptyToNonEmpty()

GlobalChangeSizeTest.test("ChangeEmptyToNonEmpty") {
  do {
    expectEqual(globalChangeEmptyToNonEmpty.property, 0)
    globalChangeEmptyToNonEmpty.property = 0xbadf00d
    expectEqual(globalChangeEmptyToNonEmpty.property, 0xbadf00d)
  }
}

var globalChangeSizeFirst = ChangeSize()
var globalChangeSizeSecond = ChangeSize()

GlobalChangeSizeTest.test("ChangeSize") {
  do {
    expectEqual(globalChangeSizeFirst.validate(), true)
    expectEqual(globalChangeSizeSecond.validate(), true)
    expectEqual(globalChangeSizeFirst.count, 0)
    expectEqual(globalChangeSizeSecond.count, 0)

    globalChangeSizeFirst.count = 101
    globalChangeSizeSecond.count = -202

    expectEqual(globalChangeSizeFirst.validate(), true)
    expectEqual(globalChangeSizeSecond.validate(), true)
    expectEqual(globalChangeSizeFirst.count, 101)
    expectEqual(globalChangeSizeSecond.count, -202)

    globalChangeSizeFirst.count = -323
    globalChangeSizeSecond.count = 545

    expectEqual(globalChangeSizeFirst.validate(), true)
    expectEqual(globalChangeSizeSecond.validate(), true)
    expectEqual(globalChangeSizeFirst.count, -323)
    expectEqual(globalChangeSizeSecond.count, 545)
  }
}

runAllTests()

