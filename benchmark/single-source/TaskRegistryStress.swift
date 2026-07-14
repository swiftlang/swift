//===--- TaskRegistryStress.swift ----------------------------------------===//
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

import TestsUtils
import Dispatch
import Foundation

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 14, iOS 17, watchOS 10.0, tvOS 17.0, *) else { return [] }
  return [
    BenchmarkInfo(
      name: "TaskRegistryStress.Serial",
      runFunction: run_serial(count: 500),
      tags: [.concurrency, .runtime]
    ),
    BenchmarkInfo(
      name: "TaskRegistryStress.Parallel",
      runFunction: run_parallel(threads: 8, tasksEach: 250),
      tags: [.concurrency, .runtime]
    ),
    BenchmarkInfo(
      name: "TaskRegistryStress.LiveSet",
      runFunction: run_liveSet(count: 500),
      tags: [.concurrency, .runtime]
    ),
    BenchmarkInfo(
      name: "TaskRegistryStress.SlidingWindow",
      runFunction: run_slidingWindow(windowSize: 100, iterations: 10000),
      tags: [.concurrency, .runtime]
    ),
  ]
}

@available(macOS 14, iOS 17, watchOS 10.0, tvOS 17.0, *)
private func run_serial(count: Int) -> (Int) -> Void {
  return { n in
    for _ in 0..<n {
      let g = DispatchGroup()
      for _ in 0..<count {
        g.enter()
        Task { g.leave() }
      }
      g.wait()
    }
  }
}

@available(macOS 14, iOS 17, watchOS 10.0, tvOS 17.0, *)
private func run_parallel(threads: Int, tasksEach: Int) -> (Int) -> Void {
  return { n in
    for _ in 0..<n {
      let outer = DispatchGroup()
      for _ in 0..<threads {
        outer.enter()
        DispatchQueue.global().async {
          let inner = DispatchGroup()
          for _ in 0..<tasksEach {
            inner.enter()
            Task { inner.leave() }
          }
          inner.wait()
          outer.leave()
        }
      }
      outer.wait()
    }
  }
}

@available(macOS 14, iOS 17, watchOS 10.0, tvOS 17.0, *)
private func run_liveSet(count: Int) -> (Int) -> Void {
  return { n in
    for _ in 0..<n {
      let lock = NSLock()
      var continuations: [UnsafeContinuation<Void, Never>] = []
      continuations.reserveCapacity(count)
      let ready = DispatchSemaphore(value: 0)
      let g = DispatchGroup()

      for _ in 0..<count {
        g.enter()
        Task {
          await withUnsafeContinuation { (cont: UnsafeContinuation<Void, Never>) in
            lock.lock()
            continuations.append(cont)
            lock.unlock()
            ready.signal()
          }
          g.leave()
        }
      }

      for _ in 0..<count { ready.wait() }
      lock.lock()
      let toResume = continuations
      lock.unlock()
      for cont in toResume { cont.resume() }
      g.wait()
    }
  }
}

@available(macOS 14, iOS 17, watchOS 10.0, tvOS 17.0, *)
private func run_slidingWindow(windowSize: Int, iterations: Int) -> (Int) -> Void {
  return { n in
    let g = DispatchGroup()
    for _ in 0..<n {
      g.enter()
      Task {
        await withDiscardingTaskGroup { group in
          for _ in 0..<windowSize {
            group.addTask { }
          }
          for _ in 0..<iterations {
            group.addTask { }
          }
        }
        g.leave()
      }
    }
    g.wait()
  }
}
