//===--- Concatenate.swift - Tests for lazy/eager concatenate -------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import StdlibCollectionUnittest


var ConcatenateTests = TestSuite("ConcatenateTests")

// Help the type checker (<rdar://problem/17897413> Slow type deduction)
typealias X = ContiguousArray<CountableRange<Int>>

let samples: ContiguousArray<(CountableRange<Int>, X)> = [
  (0..<8, [ 1..<1, 0..<5, 7..<7, 5..<7, 7..<8 ] as X),
  (0..<8, [ 0..<5, 7..<7, 5..<7, 7..<8 ] as X),
  (0..<8, [ 1..<1, 0..<5, 7..<7, 5..<7, 7..<8, 11..<11 ] as X),
  (0..<8, [ 0..<5, 7..<7, 5..<7, 7..<8, 11..<11 ] as X),
  (0..<0, [ 11..<11 ] as X),
  (0..<0, [ 3..<3, 11..<11 ] as X),
  (0..<0, [] as X),
]

for (expected, source) in samples {
  ConcatenateTests.test("forward-\(source)") {
    checkBidirectionalCollection(expected, source.joined())
  }

  ConcatenateTests.test("reverse-\(source)") {
    // FIXME: separate 'expected' and 'reversed' variables are a workaround
    // for: <rdar://problem/20789500>
    let expected = ContiguousArray(expected.lazy.reversed())
    let reversed = source.joined().reversed()
    checkBidirectionalCollection(expected, reversed)
  }

  ConcatenateTests.test("sequence-\(source)") {
    checkSequence(
      ContiguousArray(expected),
      AnySequence(source).joined())
  }
}

runAllTests()

