//===--- RandomValues.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

//
// Benchmark generating lots of random values. Measures the performance of
// the default random generator and the algorithms for generating integers
// and floating-point values.
//

public let RandomValues = [
  BenchmarkInfo(name: "RandomIntegersDef", runFunction: run_RandomIntegersDef, tags: [.api]),
  BenchmarkInfo(name: "RandomIntegersLCG", runFunction: run_RandomIntegersLCG, tags: [.api]),
  BenchmarkInfo(name: "RandomDoubleDef", runFunction: run_RandomDoubleDef, tags: [.api]),
  BenchmarkInfo(name: "RandomDoubleLCG", runFunction: run_RandomDoubleLCG, tags: [.api]),
]

/// A linear congruential PRNG.
struct LCRNG: RandomNumberGenerator {
  private var state: UInt64
  
  init(seed: Int) {
    state = UInt64(truncatingIfNeeded: seed)
    for _ in 0..<10 { _ = next() }
  }
  
  mutating func next() -> UInt64 {
    state = 2862933555777941757 &* state &+ 3037000493
    return state
  }
}

@inline(never)
public func run_RandomIntegersDef(_ N: Int) {
  for _ in 0 ..< N {
    var x = 0
    for _ in 0 ..< 100_000 {
      x &+= Int.random(in: 0...10_000)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomIntegersLCG(_ N: Int) {
  for _ in 0 ..< N {
    var x: Int64 = 0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x &+= Int64.random(in: 0...10_000, using: &generator)
    }
    CheckResults(x == 498214315)
  }
}

@inline(never)
public func run_RandomDoubleDef(_ N: Int) {
  for _ in 0 ..< N {
    var x = 0.0
    for _ in 0 ..< 100_000 {
      x += Double.random(in: -1000...1000)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomDoubleLCG(_ N: Int) {
  for _ in 0 ..< N {
    var x = 0.0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x += Double.random(in: -1000...1000, using: &generator)
    }
    blackHole(x)
  }
}
