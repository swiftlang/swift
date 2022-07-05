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

public let benchmarks = [
  BenchmarkInfo(name: "RandomIntegersDef", runFunction: run_RandomIntegersDef,
    tags: [.api], legacyFactor: 100),
  BenchmarkInfo(name: "RandomIntegersLCG", runFunction: run_RandomIntegersLCG,
    tags: [.api]),
  BenchmarkInfo(name: "RandomInt8Def", runFunction: run_RandomInt8Def,
    tags: [.api], legacyFactor: 100),
  BenchmarkInfo(name: "RandomInt8LCG", runFunction: run_RandomInt8LCG,
    tags: [.api]),
  BenchmarkInfo(name: "RandomInt64Def", runFunction: run_RandomInt64Def,
    tags: [.api], legacyFactor: 100),
  BenchmarkInfo(name: "RandomInt64LCG", runFunction: run_RandomInt64LCG,
    tags: [.api]),
  BenchmarkInfo(name: "RandomDoubleDef", runFunction: run_RandomDoubleDef,
    tags: [.api], legacyFactor: 100),
  BenchmarkInfo(name: "RandomDoubleLCG", runFunction: run_RandomDoubleLCG,
    tags: [.api]),
  BenchmarkInfo(name: "RandomDoubleOpaqueDef", runFunction: run_RandomDoubleOpaqueDef,
    tags: [.api], legacyFactor: 100),
  BenchmarkInfo(name: "RandomDoubleOpaqueLCG", runFunction: run_RandomDoubleOpaqueLCG,
    tags: [.api]),
  BenchmarkInfo(name: "RandomDouble01Def", runFunction: run_RandomDouble01Def,
    tags: [.api], legacyFactor: 100),
  BenchmarkInfo(name: "RandomDouble01LCG", runFunction: run_RandomDouble01LCG,
    tags: [.api]),
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
public func run_RandomIntegersDef(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0
    for _ in 0 ..< 1_000 {
      x &+= .random(in: 0...10_000)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomIntegersLCG(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x &+= .random(in: 0 ... 10_000, using: &generator)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomInt8Def(_ n: Int) {
  for _ in 0 ..< n {
    var x: Int8 = 0
    for _ in 0 ..< 1_000 {
      x &+= .random(in: -65 ... identity(65))
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomInt8LCG(_ n: Int) {
  for _ in 0 ..< n {
    var x: Int8 = 0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x &+= .random(in: -65 ... identity(65), using: &generator)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomInt64Def(_ n: Int) {
  for _ in 0 ..< n {
    var x: Int64 = 0
    for _ in 0 ..< 1_000 {
      x &+= .random(in:
        -5_000_000_000_000_000_000 ... identity(5_000_000_000_000_000_000)
      )
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomInt64LCG(_ n: Int) {
  for _ in 0 ..< n {
    var x: Int64 = 0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x &+= .random(in:
        -5_000_000_000_000_000_000 ... identity(5_000_000_000_000_000_000),
        using: &generator
      )
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomDoubleDef(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0.0
    for _ in 0 ..< 1_000 {
      x += .random(in: -1000 ... 1000)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomDoubleLCG(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0.0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x += .random(in: -1000 ... 1000, using: &generator)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomDoubleOpaqueDef(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0.0
    for _ in 0 ..< 1_000 {
      x += .random(in: -1000 ... identity(1000))
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomDoubleOpaqueLCG(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0.0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x += .random(in: -1000 ... identity(1000), using: &generator)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomDouble01Def(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0.0
    for _ in 0 ..< 1_000 {
      x += .random(in: 0 ..< 1)
    }
    blackHole(x)
  }
}

@inline(never)
public func run_RandomDouble01LCG(_ n: Int) {
  for _ in 0 ..< n {
    var x = 0.0
    var generator = LCRNG(seed: 0)
    for _ in 0 ..< 100_000 {
      x += .random(in: 0 ..< 1, using: &generator)
    }
    blackHole(x)
  }
}
 
