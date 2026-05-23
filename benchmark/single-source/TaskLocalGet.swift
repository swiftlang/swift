//===--- TaskLocalGet.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Measures cross-module TaskLocal.get() performance.

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 14, iOS 17, tvOS 17, watchOS 10, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "TaskLocalGet.Unbound",
      runFunction: run_TaskLocalGetUnbound,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskLocalGet.Bound",
      runFunction: run_TaskLocalGetBound,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskLocalGet.Bound.Nested",
      runFunction: run_TaskLocalGetBoundNested,
      tags: [.concurrency]
    ),
  ]
}

@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private enum BenchTaskLocals {
  @TaskLocal static var value: Int = 0
  @TaskLocal static var one: Int = 0
  @TaskLocal static var two: Int = 0
  @TaskLocal static var three: Int = 0
}

// Read a task-local that has no binding (returns defaultValue).
// This is the fast path — just a null check + return default.
@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_TaskLocalGetUnbound(_ n: Int) {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      blackHole(BenchTaskLocals.value)
    }
  }
}

// Direct read a single binding.
@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_TaskLocalGetBound(_ n: Int) {
  BenchTaskLocals.$value.withValue(42) {
    for _ in 0..<n {
      for _ in 0..<10_000 {
        blackHole(BenchTaskLocals.value)
      }
    }
  }
}

// Read a task-local with nested bindings (3 deep).
// Tests the traversal cost.
@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_TaskLocalGetBoundNested(_ n: Int) {
  BenchTaskLocals.$one.withValue(1) {
    BenchTaskLocals.$two.withValue(2) {
      BenchTaskLocals.$three.withValue(3) {
        for _ in 0..<n {
          for _ in 0..<10_000 {
            blackHole(BenchTaskLocals.one)
          }
        }
      }
    }
  }
}
