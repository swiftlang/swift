//===--- RangeContains.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "RangeContainsRange",
    runFunction: run_RangeContainsRange,
    tags: [.validation, .api],
    setUpFunction: buildRanges),
  BenchmarkInfo(
    name: "RangeContainsClosedRange",
    runFunction: run_RangeContainsClosedRange,
    tags: [.validation, .api],
    setUpFunction: buildRanges),
  BenchmarkInfo(
    name: "ClosedRangeContainsRange",
    runFunction: run_ClosedRangeContainsRange,
    tags: [.validation, .api],
    setUpFunction: buildRanges),
  BenchmarkInfo(
    name: "ClosedRangeContainsClosedRange",
    runFunction: run_ClosedRangeContainsClosedRange,
    tags: [.validation, .api],
    setUpFunction: buildRanges),
]

private func buildRanges() {
  blackHole(ranges)
  blackHole(closedRanges)
}

private let ranges: [Range<Int>] = (-8...8).flatMap { a in (0...16).map { l in a..<(a+l) } }
private let closedRanges: [ClosedRange<Int>] = (-8...8).flatMap { a in (0...16).map { l in a...(a+l) } }

@inline(never)
public func run_RangeContainsRange(_ n: Int) {
  var checksum: UInt64 = 0
  for _ in 0..<n {
    for lhs in ranges {
      for rhs in ranges {
        if lhs.contains(rhs) { checksum += 1 }
      }
    }
  }
  check(checksum == 15725 * UInt64(n))
}

@inline(never)
public func run_RangeContainsClosedRange(_ n: Int) {
  var checksum: UInt64 = 0
  for _ in 0..<n {
    for lhs in ranges {
      for rhs in closedRanges {
        if lhs.contains(rhs) { checksum += 1 }
      }
    }
  }
  check(checksum == 10812 * UInt64(n))
}

@inline(never)
public func run_ClosedRangeContainsRange(_ n: Int) {
  var checksum: UInt64 = 0
  for _ in 0..<n {
    for lhs in closedRanges {
      for rhs in ranges {
        if lhs.contains(rhs) { checksum += 1 }
      }
    }
  }
  check(checksum == 17493 * UInt64(n))
}

@inline(never)
public func run_ClosedRangeContainsClosedRange(_ n: Int) {
  var checksum: UInt64 = 0
  for _ in 0..<n {
    for lhs in closedRanges {
      for rhs in closedRanges {
        if lhs.contains(rhs) { checksum += 1 }
      }
    }
  }
  check(checksum == 12597 * UInt64(n))
}
