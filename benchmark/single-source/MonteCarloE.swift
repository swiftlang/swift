//===--- MonteCarloE.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This test measures performance of Monte Carlo estimation of the e constant.
//
// We use 'dart' method: we split an interval into N pieces and drop N darts
// to this interval.
// After that we count number of empty intervals. The probability of being
// empty is (1 - 1/N)^N which estimates to e^-1 for large N.
// Thus, e = N / Nempty.
import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "MonteCarloE",
    runFunction: run_MonteCarloE,
    tags: [.validation, .algorithm],
    legacyFactor: 20)

public func run_MonteCarloE(scale: Int) {
  var lfsr = LFSR()

  let n = 10_000 * scale
  var intervals = [Bool](repeating: false, count: n)
  for _ in 1...n {
    let pos = Int(UInt(truncatingIfNeeded: lfsr.next()) % UInt(n))
    intervals[pos] = true
  }
  let numEmptyIntervals = intervals.filter{!$0}.count
  // If there are no empty intervals, then obviously the random generator is
  // not 'random' enough.
  check(numEmptyIntervals != n)
  let e_estimate = Double(n)/Double(numEmptyIntervals)
  let e = 2.71828
  check(abs(e_estimate - e) < 0.2)
}
