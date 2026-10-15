//===--- CompactMap.swift - tests for lazy compactMap --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


let CompactMapTests = TestSuite("LazyCompactMap")

CompactMapTests.test("first evaluates the winning transform once") {
  // `lazy.compactMap` is a map/filter/map composition. The default
  // `Collection.first` computes `startIndex` (evaluating the transform on
  // the winner and discarding the result) and then subscripts that index
  // (evaluating it again). The lazy `first` overrides below pull from an
  // iterator instead, so the winner is constructed exactly once.
  var log: [String] = []
  let x: [String?] = ["1", "2", nil, "3"]
  let first = x.lazy.compactMap { (s: String?) -> String? in
    guard let s else { return nil }
    log.append(s)
    return "T(\(s))"
  }.first
  expectEqual("T(1)", first)
  expectEqual(["1"], log)
}

CompactMapTests.test("first on empty and all-nil") {
  let empty: [String?] = []
  expectNil(empty.lazy.compactMap { $0 }.first)

  var count = 0
  let nils: [String?] = [nil, nil]
  expectNil(nils.lazy.compactMap { (s: String?) -> String? in
    count += 1
    return s
  }.first)
  // `nil` inputs still pass through the transform once during the scan.
  expectEqual(2, count)
}

CompactMapTests.test("map-then-filter first evaluates once per element") {
  var mapCount = 0
  var filterCount = 0
  let first = (0..<30).lazy
    .map { (i: Int) -> Int in mapCount += 1; return i * 2 }
    .filter { (i: Int) -> Bool in filterCount += 1; return i % 7 == 0 }
    .first
  expectEqual(0, first)
  expectEqual(1, mapCount)
  expectEqual(1, filterCount)
}

CompactMapTests.test("filter-then-map first evaluates once per element") {
  var mapCount = 0
  var filterCount = 0
  let first = (0..<30).lazy
    .filter { (i: Int) -> Bool in filterCount += 1; return i % 7 == 0 }
    .map { (i: Int) -> Int in mapCount += 1; return i * 2 }
    .first
  expectEqual(0, first)
  expectEqual(1, filterCount)
  expectEqual(1, mapCount)
}

CompactMapTests.test("plain lazy map first evaluates once") {
  var count = 0
  let first = (0..<30).lazy.map { (i: Int) -> Int in
    count += 1
    return i * 2
  }.first
  expectEqual(0, first)
  expectEqual(1, count)
}

CompactMapTests.test("plain lazy filter first evaluates once per visit") {
  var count = 0
  let first = (0..<30).lazy.filter { (i: Int) -> Bool in
    count += 1
    return i % 7 == 0
  }.first
  expectEqual(0, first)
  expectEqual(1, count)
}

CompactMapTests.test("full traversal values are unchanged") {
  expectEqualSequence(
    [0, 7, 14, 21, 28],
    (0..<30).lazy.compactMap { (i: Int) -> Int? in
      i % 7 == 0 ? i : nil
    })
  expectEqualSequence(
    ["T(a)", "T(b)"],
    ([nil, "a", nil, "b", "c"] as [String?]).lazy
      .compactMap { $0.map { "T(\($0))" } }
      .prefix(2))
  expectEqualSequence(
    [0, 2, 4],
    (0..<5).lazy.compactMap { $0 % 2 == 0 ? $0 : nil })
}

CompactMapTests.test("index traversal agrees with iteration") {
  let c = (0..<5).lazy.compactMap { $0 % 2 == 0 ? $0 : nil }
  var walked: [Int] = []
  var i = c.startIndex
  while i != c.endIndex {
    walked.append(c[i])
    c.formIndex(after: &i)
  }
  expectEqual([0, 2, 4], walked)
}

CompactMapTests.test("sequences evaluate once per element") {
  var count = 0
  let s = (0..<30).makeIterator().lazy.compactMap { (i: Int) -> Int? in
    count += 1
    return i % 7 == 0 ? i : nil
  }
  expectEqualSequence([0, 7, 14, 21, 28], s)
  expectEqual(30, count)
}

runAllTests()
