//===--- RangeAssignment.swift --------------------------------------------===//
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

import TestsUtils

#if swift(>=4.2)
public let RangeIteration = [
  BenchmarkInfo(
    name: "RangeIterationSigned",
    runFunction: run_RangeIterationSigned,
    tags: [.validation, .api]
  ),
  BenchmarkInfo(
    name: "RangeIterationSigned64",
    runFunction: run_RangeIterationSigned64,
    tags: [.validation, .api]
  ),
  BenchmarkInfo(
    name: "RangeIterationUnsigned",
    runFunction: run_RangeIterationUnsigned,
    tags: [.validation, .api]
  ),
]
#else
public let RangeIteration = [
  BenchmarkInfo(
    name: "RangeIterationSigned",
    runFunction: run_RangeIterationSigned,
    tags: [.validation, .api]
  )
]
#endif

public var check: UInt64 = 0

@inline(never)
func sum(_ x: UInt64, _ y: UInt64) -> UInt64 {
  return x &+ y
}

@inline(never)
public func run_RangeIterationSigned(_ N: Int) {
  let range = 0..<100000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
    }
  }

  CheckResults(check == 4999950000 * UInt64(N))
}

#if swift(>=4.2)

@inline(never)
public func run_RangeIterationSigned64(_ N: Int) {
  let range: Range<Int64> = 0..<100000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
    }
  }

  CheckResults(check == 4999950000 * UInt64(N))
}

@inline(never)
public func run_RangeIterationUnsigned(_ N: Int) {
  let range: Range<UInt> = 0..<100000
  check = 0
  for _ in 1...N {
    for e in range {
      check = sum(check, UInt64(e))
    }
  }

  CheckResults(check == 4999950000 * UInt64(N))
}

#endif
