//===--- ParameterPacks.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This benchmark serves to test the overhead of

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(
    name: "ParameterPacks.VariadicFibonacci",
    runFunction: run_ParameterPacksVariadicFibonacci,
    tags: [.validation])
]

@inline(never)
func numericLoop<each T: BinaryInteger>(xs: repeat each T) -> (repeat each T) {
  return
    (repeat { x in
      // Recursive "Fibonacci" function prevents inlining
      if x <= 1 {
        return x
      } else {
        let (a, b) = numericLoop(xs: x - 1, x - 2)
        return a + b
      }
    }(each xs)

  )
}

let expectedResult = (610, 987, 1597)

@inline(never)
public func run_ParameterPacksVariadicFibonacci(_ n: Int) {
  var result = (0, 0, 0)
  for _ in 1...n {
    result = numericLoop(xs: 15, 16, 17)
    if result != expectedResult {
      break
    }
  }

  check(result == expectedResult)
}
