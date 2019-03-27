//===--- RangeOverlaps.swift ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let RangeOverlaps = [
  BenchmarkInfo(
    name: "RangeOverlapsRange",
    runFunction: run_RangeOverlapsRange,
    tags: [.validation, .api],
    setUpFunction: buildRanges),
  BenchmarkInfo(
    name: "RangeOverlapsClosedRange",
    runFunction: run_RangeOverlapsClosedRange,
    tags: [.validation, .api],
    setUpFunction: buildRanges),
  BenchmarkInfo(
    name: "ClosedRangeOverlapsClosedRange",
    runFunction: run_ClosedRangeOverlapsClosedRange,
    tags: [.validation, .api],
    setUpFunction: buildRanges)
]

private func buildRanges() {
  blackHole(ranges)
  blackHole(closedRanges)
}

private let ranges: [Range<Int>] = (-8...8).flatMap { a in (0...16).map { l in a..<(a+l) } }
private let closedRanges: [ClosedRange<Int>] = (-8...8).flatMap { a in (0...16).map { l in a...(a+l) } }

@inline(never)
public func run_RangeOverlapsRange(_ N: Int) {
  var check: UInt64 = 0
  for _ in 0..<N {
    for lhs in ranges {
      for rhs in ranges {
        if lhs.overlaps(rhs) { check += 1 }
      }
    }
  }
  CheckResults(check == 47872 * UInt64(N))
}

@inline(never)
public func run_RangeOverlapsClosedRange(_ N: Int) {
  var check: UInt64 = 0
  for _ in 0..<N {
    for lhs in ranges {
      for rhs in closedRanges {
        if lhs.overlaps(rhs) { check += 1 }
      }
    }
  }
  CheckResults(check == 51680 * UInt64(N))
}

@inline(never)
public func run_ClosedRangeOverlapsClosedRange(_ N: Int) {
  var check: UInt64 = 0
  for _ in 0..<N {
    for lhs in closedRanges {
      for rhs in closedRanges {
        if lhs.overlaps(rhs) { check += 1 }
      }
    }
  }
  CheckResults(check == 55777 * UInt64(N))
}
