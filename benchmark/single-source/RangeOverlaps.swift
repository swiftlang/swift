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
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "RangeOverlapsClosedRange",
    runFunction: run_RangeOverlapsClosedRange,
    tags: [.validation, .api]),
  BenchmarkInfo(
    name: "ClosedRangeOverlapsClosedRange",
    runFunction: run_ClosedRangeOverlapsClosedRange,
    tags: [.validation, .api]),
]

@inline(never) func foo() { }

@inline(never)
public func run_RangeOverlapsRange(_ N: Int) {
  var ranges: [Range<Int>] = []
  for a in -20 ... 20 {
    for b in 0 ... 40 {
      ranges.append(a ..< (a+b))
    }
  }
  var check: UInt64 = 0
  for _ in 0..<N {
    for lhs in ranges {
      for rhs in ranges {
        if lhs.overlaps(rhs) { check += 1 }
      }
    }
  }
  CheckResults(check == 1771200 * UInt64(N))
}

@inline(never)
public func run_RangeOverlapsClosedRange(_ N: Int) {
  var ranges: [Range<Int>] = []
  var closedRanges: [ClosedRange<Int>] = []
  for a in -20 ... 20 {
    for b in 0 ... 40 {
      ranges.append(a ..< (a+b))
      closedRanges.append(a ... (a+b))
    }
  }
  var check: UInt64 = 0
  for _ in 0..<N {
    for lhs in ranges {
      for rhs in closedRanges {
        if lhs.overlaps(rhs) { check += 1 }
      }
    }
  }
  CheckResults(check == 1826960 * UInt64(N))
}

@inline(never)
public func run_ClosedRangeOverlapsClosedRange(_ N: Int) {
  var closedRanges: [ClosedRange<Int>] = []
  for a in -20 ... 20 {
    for b in 0 ... 40 {
      closedRanges.append(a ... (a+b))
    }
  }
  var check: UInt64 = 0
  for _ in 0..<N {
    for lhs in closedRanges {
      for rhs in closedRanges {
        if lhs.overlaps(rhs) { check += 1 }
      }
    }
  }
  CheckResults(check == 1884401 * UInt64(N))
}
