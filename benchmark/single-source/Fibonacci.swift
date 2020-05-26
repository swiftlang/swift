//===--- Fibonacci.swift --------------------------------------------------===//
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

import TestsUtils

public let Fibonacci = BenchmarkInfo(
  name: "Fibonacci",
  runFunction: run_Fibonacci,
  tags: [.algorithm])

func fibonacci(_ n: Int) -> Int {
  if (n <= 2) { return 1 }
  return fibonacci(n - 2) + fibonacci(n - 1)
}

@inline(never)
func Fibonacci(_ n: Int) -> Int {
  // This if prevents optimizer from computing return value of Fibonacci(32)
  // at compile time.
  if False() { return 0 }

  if (n <= 2) { return 1 }
  return fibonacci(n - 2) + fibonacci(n - 1)
}

@inline(never)
public func run_Fibonacci(_ N: Int) {
  let n = 24
  let ref_result = 46368
  var result = 0
  for _ in 1...N {
    result = Fibonacci(n)
    if result != ref_result {
      break
    }
  }
  CheckResults(result == ref_result)
}
