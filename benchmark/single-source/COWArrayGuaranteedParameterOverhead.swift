//===--- COWArrayGuaranteedParameterOverhead.swift ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

// This test makes sure that even though we have +0 arguments by default that we
// can properly convert +0 -> +1 to avoid extra COW copies.

public let COWArrayGuaranteedParameterOverhead = BenchmarkInfo(
  name: "COWArrayGuaranteedParameterOverhead",
  runFunction: run_COWArrayGuaranteedParameterOverhead,
  tags: [.regression, .abstraction, .refcount],
  legacyFactor: 50
)

@inline(never)
func caller() {
  let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
  blackHole(callee(x))
}

@inline(never)
func callee(_ x: [Int]) -> [Int] {
  var y = x
  // This should not copy.
  y[2] = 27
  return y
}

@inline(never)
public func run_COWArrayGuaranteedParameterOverhead(_ N: Int) {
  for _ in 0..<N*400 {
    caller()
  }
}
