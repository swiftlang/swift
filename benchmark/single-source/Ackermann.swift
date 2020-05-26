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

public let Ackermann = BenchmarkInfo(
  name: "Ackermann",
  runFunction: run_Ackermann,
  tags: [.algorithm])

func ackermann(_ M: Int, _ N : Int) -> Int {
  if (M == 0) { return N + 1 }
  if (N == 0) { return ackermann(M - 1, 1) }
  return ackermann(M - 1, ackermann(M, N - 1))
}

@inline(never)
func Ackermann(_ M: Int, _ N : Int) -> Int {
  // This if prevents optimizer from computing return value of Ackermann(3,9)
  // at compile time.
  if False() { return 0 }
  if (M == 0) { return N + 1 }
  if (N == 0) { return ackermann(M - 1, 1) }
  return ackermann(M - 1, ackermann(M, N - 1))
}

let ref_result = [5, 13, 29, 61, 125, 253, 509, 1021, 2045, 4093, 8189, 16381, 32765, 65533, 131069]

@inline(never)
public func run_Ackermann(_ N: Int) {
  let (m, n) = (3, 6)
  var result = 0
  for _ in 1...N {
    result = Ackermann(m, n)
    if result != ref_result[n] {
      break
    }
  }
  CheckResults(result == ref_result[n])
}
