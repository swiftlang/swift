//===--- Ackermann.swift --------------------------------------------------===//
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

// This test is based on test Ackermann from utils/benchmark, with modifications
// for performance measuring.
import TestsUtils

public let benchmarks: [Benchmark] = [
  Ackermann()
]

struct Ackermann: Benchmark {
  var name: String { "Ackermann" }
  var tags: Tags { [.algorithm] }

  let ref_result = [
    5, 13, 29, 61, 125, 253, 509, 1021, 2045,
    4093, 8189, 16381, 32765, 65533, 131069
  ]

  func run(iterations: Int) {
    let (m, n) = (3, 6)
    var result = 0
    for _ in 1 ... iterations {
      result = ackermann(m, n)
      if result != ref_result[n] {
        break
      }
    }
    check(result == ref_result[n])
  }

  @inline(never)
  func ackermann(_ m: Int, _ n : Int) -> Int {
    // This if prevents optimizer from computing the return value at compile
    // time.
    if getFalse() { return 0 }
    if m == 0 { return n + 1 }
    if n == 0 { return ackermann(m - 1, 1) }
    return ackermann(m - 1, ackermann(m, n - 1))
  }

  func _ackermann(_ m: Int, _ n : Int) -> Int {
    if m == 0 { return n + 1 }
    if n == 0 { return ackermann(m - 1, 1) }
    return ackermann(m - 1, ackermann(m, n - 1))
  }
}
