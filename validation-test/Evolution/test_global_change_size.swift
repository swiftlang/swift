// RUN: rm -rf %t && mkdir -p %t/before && mkdir -p %t/after

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/global_change_size.swift -o %t/before/global_change_size.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D BEFORE -c %S/Inputs/global_change_size.swift -o %t/before/global_change_size.o

// RUN: %target-build-swift -emit-library -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/global_change_size.swift -o %t/after/global_change_size.o
// RUN: %target-build-swift -emit-module -Xfrontend -enable-resilience -D AFTER -c %S/Inputs/global_change_size.swift -o %t/after/global_change_size.o

// RUN: %target-build-swift -D BEFORE -c %s -I %t/before -o %t/before/main.o
// RUN: %target-build-swift -D AFTER -c %s -I %t/after -o %t/after/main.o

// RUN: %target-build-swift %t/before/global_change_size.o %t/before/main.o -o %t/before_before
// RUN: %target-build-swift %t/before/global_change_size.o %t/after/main.o -o %t/before_after
// RUN: %target-build-swift %t/after/global_change_size.o %t/before/main.o -o %t/after_before
// RUN: %target-build-swift %t/after/global_change_size.o %t/after/main.o -o %t/after_after

// RUN: %target-run %t/before_before
// RUN: %target-run %t/before_after
// RUN: %target-run %t/after_before
// RUN: %target-run %t/after_after

import StdlibUnittest
import global_change_size

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

