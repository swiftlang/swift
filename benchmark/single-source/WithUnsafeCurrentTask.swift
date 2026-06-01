//===--- WithUnsafeCurrentTask.swift --------------------------------------===//
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

// Measures the cost of `withUnsafeCurrentTask`.

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 14, iOS 17, tvOS 17, watchOS 10, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "WithUnsafeCurrentTask.Empty",
      runFunction: run_WithUnsafeCurrentTask_Empty,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "WithUnsafeCurrentTask.Empty.NoTask",
      runFunction: run_WithUnsafeCurrentTask_NoTask,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "WithUnsafeCurrentTask.Priority",
      runFunction: run_WithUnsafeCurrentTask_Priority,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "WithUnsafeCurrentTask.IsCancelled",
      runFunction: run_WithUnsafeCurrentTask_IsCancelled,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "WithUnsafeCurrentTask.Hash",
      runFunction: run_WithUnsafeCurrentTask_Hash,
      tags: [.concurrency]
    ),
  ]
}

// Empty closure body — isolates the cost of obtaining an `UnsafeCurrentTask`.
@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_WithUnsafeCurrentTask_Empty(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      withUnsafeCurrentTask { task in
        blackHole(task != nil)
      }
    }
  }
}

// Empty closure body invoked outside any Task — measures the nil-return fast
// path (`swift_task_getCurrent` returning null + a body call with `nil`).
@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_WithUnsafeCurrentTask_NoTask(_ n: Int) {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      withUnsafeCurrentTask { task in
        blackHole(task == nil)
      }
    }
  }
}

// Read `priority` inside the closure — exercises a runtime call on the
// task pointer in addition to the acquisition cost.
@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_WithUnsafeCurrentTask_Priority(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      withUnsafeCurrentTask { task in
        if let p = task?.priority {
          blackHole(p.rawValue)
        }
      }
    }
  }
}

@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_WithUnsafeCurrentTask_IsCancelled(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      withUnsafeCurrentTask { task in
        blackHole(task?.isCancelled == true)
      }
    }
  }
}

// Hash the `UnsafeCurrentTask` value — exercises the value-witness path
// that previously had to bridge through `Builtin.bridgeToRawPointer`.
@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_WithUnsafeCurrentTask_Hash(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      withUnsafeCurrentTask { task in
        guard let task = task else { return }
        var h = Hasher()
        task.hash(into: &h)
        blackHole(h.finalize())
      }
    }
  }
}
