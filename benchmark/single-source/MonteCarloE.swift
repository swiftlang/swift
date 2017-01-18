//===--- MonteCarloE.swift ------------------------------------------------===//
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

// This test measures performance of Monte Carlo estimation of the e constant.
//
// We use 'dart' method: we split an interval into N pieces and drop N darts
// to this interval.
// After that we count number of empty intervals. The probability of being
// empty is (1 - 1/N)^N which estimates to e^-1 for large N.
// Thus, e = N / Nempty.
import TestsUtils

public func run_MonteCarloE(scale: Int) {
  let N = 200000*scale
  var intervals = [Bool](repeating: false, count: N)
  for _ in 1...N {
    let pos = Int(UInt(truncatingBitPattern: Random())%UInt(N))
    intervals[pos] = true
  }
  let numEmptyIntervals = intervals.filter{!$0}.count
  // If there are no empty intervals, then obviously the random generator is
  // not 'random' enough.
  CheckResults(numEmptyIntervals != N,
               "Incorrect results in MonteCarloE: no empty intervals.")
  let e_estimate = Double(N)/Double(numEmptyIntervals)
  let e = 2.71828
  CheckResults(abs(e_estimate - e) < 0.1,
               "Incorrect results in MonteCarloE: e_estimate == \(e_estimate)")
}
