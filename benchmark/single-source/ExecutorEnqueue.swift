//===--- ExecutorEnqueue.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Compares actor hop overhead when a SerialExecutor implements the legacy
// enqueue(_ job: UnownedJob) overload versus the preferred
// enqueue(_ job: consuming ExecutorJob) overload (SE-0392, Swift 5.9).
//
//===----------------------------------------------------------------------===//

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "ExecutorEnqueue.UnownedJob",
      runFunction: run_UnownedJobActor,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "ExecutorEnqueue.ExecutorJob",
      runFunction: run_ExecutorJobActor,
      tags: [.concurrency]
    ),
  ]
}

// MARK: - Executors

/// Implements the legacy enqueue(_ job: UnownedJob) overload.
@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
private final class InlineUnownedJobExecutor: SerialExecutor, @unchecked Sendable {
  // Implementing the deprecated enqueue(UnownedJob) overload is intentional;
  // the benchmark measures the overhead of this delegation path.
  func enqueue(_ job: UnownedJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
}

/// Implements the preferred enqueue(_ job: consuming ExecutorJob) overload.
@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
private final class InlineExecutorJobExecutor: SerialExecutor, @unchecked Sendable {
  func enqueue(_ job: consuming ExecutorJob) {
    job.runSynchronously(on: self.asUnownedSerialExecutor())
  }
}

// MARK: - Actors

@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
private actor UnownedJobActor {
  private var counter: Int = 0
  private let _executor = InlineUnownedJobExecutor()

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    _executor.asUnownedSerialExecutor()
  }

  func increment() {
    counter &+= 1
  }
}

@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
private actor ExecutorJobActor {
  private var counter: Int = 0
  private let _executor = InlineExecutorJobExecutor()

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    _executor.asUnownedSerialExecutor()
  }

  func increment() {
    counter &+= 1
  }
}

// MARK: - Benchmark functions

@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
private func run_UnownedJobActor(_ n: Int) async {
  let a = UnownedJobActor()
  for _ in 0 ..< n {
    await a.increment()
  }
}

@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
private func run_ExecutorJobActor(_ n: Int) async {
  let a = ExecutorJobActor()
  for _ in 0 ..< n {
    await a.increment()
  }
}
