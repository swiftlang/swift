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
    tags: [.api], setUpFunction: { blackHole(numbersDef) }, legacyFactor: 4),
  BenchmarkInfo(name: "RandomShuffleLCG2", runFunction: run_RandomShuffleLCG,
    tags: [.api], setUpFunction: { blackHole(numbersLCG) }, legacyFactor: 16),
  BenchmarkInfo(name: "RandomShuffleReference", runFunction: run_RandomShuffleReference,
    tags: [.api], setUpFunction: { blackHole(numbersReference) }),
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

class IntRef: Comparable {
    var value: Int

    init(_ v: Int) { value = v }

    static func ==(lhs: IntRef, rhs: IntRef) -> Bool {
        lhs.value == rhs.value
    }

    static func < (lhs: IntRef, rhs: IntRef) -> Bool {
        lhs.value < rhs.value
    }
}

var numbersDef: [Int] = Array(0...2_500)
var numbersLCG: [Int] = Array(0...6_250)
var numbersReference: [IntRef] = (0...2_500).map(IntRef.init)

@inline(never)
public func run_RandomShuffleDef(_ N: Int) {
  for _ in 0 ..< N {
    numbersDef.shuffle()
    blackHole(numbersDef.first!)
  }
}

@inline(never)
public func run_RandomShuffleLCG(_ N: Int) {
  var generator = LCRNG(seed: 0)
  for _ in 0 ..< N {
    numbersLCG.shuffle(using: &generator)
    blackHole(numbersLCG.first!)
  }
}

@inline(never)
public func run_RandomShuffleReference(_ N: Int) {
  for _ in 0 ..< N {
    numbersReference.shuffle()
    blackHole(numbersReference.first!)
  }
}
