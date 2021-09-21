//===--- MonteCarloPi.swift -----------------------------------------------===//
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

import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "MonteCarloPi",
    runFunction: run_MonteCarloPi,
    tags: [.validation, .algorithm],
    legacyFactor: 125)

public func run_MonteCarloPi(scale: Int) {
  var rng = LFSR()

  var pointsInside = 0
  let r = 10000
  let n = 4_000*scale
  for _ in 1...n {
    let x = Int(truncatingIfNeeded: rng.next()) % r
    let y = Int(truncatingIfNeeded: rng.next()) % r
    if x*x + y*y < r*r {
      pointsInside += 1
    }
  }
  let pi_estimate: Double = Double(pointsInside)*4.0/Double(n)
  let pi = 3.1415
  check(abs(pi_estimate - pi) < 0.2)
}
