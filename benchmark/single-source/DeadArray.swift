//===--- DeadArray.swift --------------------------------------------------===//
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

// rdar://problem/20980377
import TestsUtils

@inline(__always)
func debug(_ m:String) {}

private var Count = 0

@inline(never)
func bar() { Count += 1 }

@inline(never)
func runLoop(_ var1: Int, var2: Int) {
  for _ in 0..<100_000 {
    debug("Var1: \(var1) Var2: \(var2)")
    bar()
  }
}

@inline(never)
public func run_DeadArray(_ N: Int) {
  for _ in 1...N {
    Count = 0
    runLoop(0, var2: 0)
  }
  CheckResults(Count == 100_000, "Incorrect number of calls in loop")
}
