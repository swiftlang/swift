//===--- ChildTaskCreation.swift -----------------------------------------===//
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

// Compare the two structured-concurrency child-task shapes:
//   - `async let` (fixed arity, statically known at compile time)
//   - `withTaskGroup` (dynamic arity)
//
// Two group variants:
//   - one child (minimal group overhead)
//   - 100 children (steady-state group churn)
//
// All child bodies do the same tiny deterministic work so the wall-clock
// difference reflects the structural overhead of each construct, not the
// per-child work. Numbers run in the same process so drive() overhead is
// shared and cancels out in ratios.

import TestsUtils
import Dispatch

public var benchmarks: [BenchmarkInfo] {
  var b: [BenchmarkInfo] = [
    BenchmarkInfo(name: "ChildTaskCreation.AsyncLet.One",
                  runFunction: run_AsyncLet_One, tags: [.concurrency]),
    BenchmarkInfo(name: "ChildTaskCreation.TaskGroup.One",
                  runFunction: run_TaskGroup_One, tags: [.concurrency]),
    BenchmarkInfo(name: "ChildTaskCreation.TaskGroup.100",
                  runFunction: run_TaskGroup_100, tags: [.concurrency]),
  ]
  if #available(macOS 14, iOS 17, tvOS 17, watchOS 10, *) {
    b.append(BenchmarkInfo(name: "ChildTaskCreation.DiscardingTaskGroup.100",
                           runFunction: run_DiscardingTaskGroup_100, tags: [.concurrency]))
  }
  return b
}

@inline(never)
private func drive(_ body: @escaping @Sendable () async -> Void) {
  let g = DispatchGroup()
  g.enter()
  Task {
    await body()
    g.leave()
  }
  g.wait()
}

// Tiny deterministic work; matches the shape used in WithDeadlineVsTaskGroup
// so cross-file comparisons are apples-to-apples.
@inline(never)
private func doWork() -> Int {
  var acc = 0
  for i in 0..<64 { acc = acc &+ i &* i }
  return acc
}

@inline(never)
public func run_AsyncLet_One(n: Int) {
  drive {
    for _ in 0..<n {
      async let child: Int = doWork()
      let value = await child
      blackHole(value)
    }
  }
}

@inline(never)
public func run_TaskGroup_One(n: Int) {
  drive {
    for _ in 0..<n {
      let value = await withTaskGroup(of: Int.self) { group in
        group.addTask { doWork() }
        return await group.next() ?? 0
      }
      blackHole(value)
    }
  }
}

@inline(never)
public func run_TaskGroup_100(n: Int) {
  drive {
    for _ in 0..<n {
      let sum = await withTaskGroup(of: Int.self) { group in
        for _ in 0..<100 {
          group.addTask { doWork() }
        }
        var acc = 0
        for await value in group {
          acc = acc &+ value
        }
        return acc
      }
      blackHole(sum)
    }
  }
}

@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
@inline(never)
public func run_DiscardingTaskGroup_100(n: Int) {
  drive {
    for _ in 0..<n {
      await withDiscardingTaskGroup { group in
        for _ in 0..<100 {
          group.addTask {
            blackHole(doWork())
          }
        }
      }
    }
  }
}
