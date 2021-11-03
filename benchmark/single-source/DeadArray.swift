//===--- DeadArray.swift --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// rdar://problem/20980377
import TestsUtils

public let benchmarks =
  BenchmarkInfo(
    name: "DeadArray",
    runFunction: run_DeadArray,
    tags: [.regression, .unstable],
    legacyFactor: 200
  )

@inline(__always)
func debug(_ m:String) {}

private var count = 0

@inline(never)
func bar() { count += 1 }

@inline(never)
func runLoop(_ var1: Int, var2: Int) {
  for _ in 0..<500 {
    debug("Var1: \(var1) Var2: \(var2)")
    bar()
  }
}

@inline(never)
public func run_DeadArray(_ n: Int) {
  for _ in 1...n {
    count = 0
    runLoop(0, var2: 0)
  }
  check(count == 500)
}
