//===--- TaskDeadlines.swift ----------------------------------------------===//
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

// Compare the built-in `withDeadline` (scope + timer, no child tasks) against
// the "naive" implementation a user would write without it.
//
// All variants use a far-future deadline so the deadline never fires,
// this benchmark is about the cost of setting up the deadlines.

@_spi(Concurrency) import _Concurrency
import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(anyAppleOS 9999, *) else { return [] }
  return [
    BenchmarkInfo(name: "WithDeadline.FarFuture.NoOp",
                  runFunction: run_WithDeadline_NoOp, tags: [.concurrency]),
    BenchmarkInfo(name: "WithDeadline.FarFuture.WorkChild",
                  runFunction: run_WithDeadline_WorkChild, tags: [.concurrency]),
    BenchmarkInfo(name: "NaiveTaskGroupDeadline.FarFuture.WorkChild",
                  runFunction: run_NaiveTaskGroupDeadline_WorkChild, tags: [.concurrency]),
  ]
}

@inline(never)
private func doWork() -> Int {
  var acc = 0
  for i in 0..<64 { acc = acc &+ i &* i }
  return acc
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_WithDeadline_NoOp(n: Int) async {
  let clock = ContinuousClock()
  for _ in 0..<n {
    let deadline = clock.now.advanced(by: .seconds(3600))
    _ = try? await withDeadline(deadline, clock: clock) {
      return 0
    }
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_WithDeadline_WorkChild(n: Int) async {
  let clock = ContinuousClock()
  for _ in 0..<n {
    let deadline = clock.now.advanced(by: .seconds(3600))
    let result = try? await withDeadline(deadline, clock: clock) {
      doWork()
    }
    blackHole(result ?? 0)
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_NaiveTaskGroupDeadline_WorkChild(n: Int) async {
  let clock = ContinuousClock()
  for _ in 0..<n {
    let deadline = clock.now.advanced(by: .seconds(3600))
    // The shape withDeadline that's implementable outside of the stdlib.
    let result = await withTaskGroup(of: Int?.self) { group in
      group.addTask { doWork() }
      group.addTask {
        try? await ContinuousClock().sleep(until: deadline, tolerance: nil)
        return nil
      }
      let first = await group.next() ?? nil
      group.cancelAll()
      return first
    }
    blackHole(result ?? 0)
  }
}
