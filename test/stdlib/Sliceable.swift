//===--- Sliceable.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift | FileCheck %s
import Darwin
import StdlibUnittest

var SliceableTests = TestCase("SliceableTests")

SliceableTests.test("dropFirstLast") {
  if true {
    let a = [2, 3, 5, 7, 11]
    expectEqual(a[a.startIndex.successor()..<a.endIndex], dropFirst(a))
    expectEqual(a[a.startIndex..<a.endIndex.predecessor()], dropLast(a))
  }
  if true {
    let a = "bird in the hand"
    expectEqual(a[a.startIndex.successor()..<a.endIndex], dropFirst(a))
    expectEqual(a[a.startIndex..<a.endIndex.predecessor()], dropLast(a))
  }
}

SliceableTests.test("prefixSuffix") {
  if true {
    let a = [2, 3, 5, 7, 11]
    let count = countElements(a)
    expectEqualSequence([], prefix(a, -10))
    expectEqualSequence([], suffix(a, -10))
    expectEqualSequence(a, prefix(a, count + 1))
    expectEqualSequence(a, prefix(a, count))
    expectEqualSequence(dropLast(a), prefix(a, count - 1))
    expectEqualSequence(dropLast(dropLast(a)), prefix(a, count - 2))
    
    expectEqualSequence(a, suffix(a, count + 1))
    expectEqualSequence(a, suffix(a, count))
    expectEqualSequence(dropFirst(a), suffix(a, count - 1))
    expectEqualSequence(dropFirst(dropFirst(a)), suffix(a, count - 2))
  }

  if true {
    let a = "bird in the hand"
    let count = countElements(a)
    expectEqualSequence("", prefix(a, -10))
    expectEqualSequence("", suffix(a, -10))
    expectEqualSequence(a, prefix(a, count + 1))
    expectEqualSequence(a, prefix(a, count))
    expectEqualSequence(dropLast(a), prefix(a, count - 1))
    expectEqualSequence(dropLast(dropLast(a)), prefix(a, count - 2))
    
    expectEqualSequence(a, suffix(a, count + 1))
    expectEqualSequence(a, suffix(a, count))
    expectEqualSequence(dropFirst(a), suffix(a, count - 1))
    expectEqualSequence(dropFirst(dropFirst(a)), suffix(a, count - 2))
  }
}

SliceableTests.run() // CHECK: {{^}}SliceableTests: All tests passed
