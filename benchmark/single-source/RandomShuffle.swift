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
  BenchmarkInfo(name: "RandomShuffleDef", runFunction: run_RandomShuffleDef,
    tags: [.api], setUpFunction: setup_RandomShuffle),
  BenchmarkInfo(name: "RandomShuffleLCG", runFunction: run_RandomShuffleLCG,
    tags: [.api], setUpFunction: setup_RandomShuffle),
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

var numbers = Array(0...3_000_000)

@inline(never)
func setup_RandomShuffle() {
  _ = numbers.count
}

@inline(never)
public func run_RandomShuffleDef(_ N: Int) {
  for _ in 0 ..< N {
    numbers.shuffle()
    blackHole(numbers.first!)
  }
}

@inline(never)
public func run_RandomShuffleLCG(_ N: Int) {
  var generator = LCRNG(seed: 0)
  for _ in 0 ..< N {
    numbers.shuffle(using: &generator)
    blackHole(numbers.first!)
  }
}
