//===--- RandomShuffle.swift ----------------------------------------------===//
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
// Benchmark that shuffles arrays of integers. Measures the performance of
// shuffling large arrays.
//

public let RandomShuffle = [
  BenchmarkInfo(name: "RandomShuffleDef2", runFunction: run_RandomShuffleDef,
    tags: [.api],
    setUpFunction: { blackHole(numbersDef) },
    tearDownFunction: { numbersDef = nil }),
  BenchmarkInfo(name: "RandomShuffleLCG2", runFunction: run_RandomShuffleLCG,
    tags: [.api],
    setUpFunction: { blackHole(numbersLCG) },
    tearDownFunction: { numbersLCG = nil }),
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

var numbersDef: [Int]! = Array(0...10_000)
var numbersLCG: [Int]! = Array(0...100_000)

@inline(never)
public func run_RandomShuffleDef(_ N: Int) {
  var numbers: [Int] = numbersDef
  for _ in 0 ..< N {
    numbers.shuffle()
    blackHole(numbers.first!)
  }
}

@inline(never)
public func run_RandomShuffleLCG(_ N: Int) {
  var numbers: [Int] = numbersLCG
  var generator = LCRNG(seed: 0)
  for _ in 0 ..< N {
    numbers.shuffle(using: &generator)
    blackHole(numbers.first!)
  }
}
