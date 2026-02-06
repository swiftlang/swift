//===--- TaskWorklist.swift -----------------------------------------------===//
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

import Foundation
import Synchronization

/// A worklist of tasks that enforces a maximum amount of paralellism.
public struct TaskWorklist<T: Sendable> : Sendable, ~Copyable {
  public let maxParallel: Int

  private struct State {
    var worklist: [@Sendable () async -> T] = []

    private(set) var results: AsyncStream<T>
    private(set) var resultCont: AsyncStream<T>.Continuation
    private(set) var generation: Int = 0

    var workers: [Int: Task<Void, Never>] = [:]

    init() {
      var cont: AsyncStream<T>.Continuation?
      self.results = AsyncStream<T> { cont = $0 }
      self.resultCont = cont!
      self.resultCont.finish()
    }

    mutating func cancel(generation: Int?) {
      guard generation == nil || generation == self.generation else { return }
      worklist.removeAll()
      for (_, task) in workers {
        task.cancel()
      }
      workers.removeAll()
      resultCont.finish()
    }

    mutating func reset(onCancel: @Sendable @escaping (Int) -> Void) {
      precondition(workers.isEmpty)

      var cont: AsyncStream<T>.Continuation?
      results = AsyncStream<T> { cont = $0 }
      resultCont = cont!
      generation &+= 1
      resultCont.onTermination = { [generation] in
        guard $0 == .cancelled else { return }
        onCancel(generation)
      }
    }
  }
  private final class Context: Sendable {
    private let state = Mutex(State())
    func withLock<R, E>(
      _ body: sending (inout sending State) throws(E) -> sending R
    ) throws(E) -> sending R {
      try state.withLock(body)
    }
    func popTask() -> (() async -> T)? {
      withLock {
        guard !Task.isCancelled else { return nil }
        return $0.worklist.popLast()
      }
    }
  }
  private let context = Context()

  public init(maxParallel: Int? = nil) {
    self.maxParallel = maxParallel ?? ProcessInfo.processInfo.processorCount * 3
  }

  private static func workerLoop(id: Int, context: Context) async {
    while let fn = context.popTask() {
      let val = await Task { await fn() }.valuePropagatingCancellation
      context.withLock { state in
        guard !Task.isCancelled else { return }
        _ = state.resultCont.yield(val)
      }
    }
    context.withLock { state in
      guard !Task.isCancelled else { return }
      state.workers[id] = nil
      if state.workers.isEmpty {
        state.resultCont.finish()
      }
    }
  }

  public func addTask(
    @_inheritActorContext _ fn: @Sendable @escaping () async -> T
  ) {
    context.withLock { state in
      state.worklist.append(fn)

      // Check to see if we can add another worker.
      guard state.workers.count < self.maxParallel else { return }

      // If the workers were previously exhausted, reset the stream.
      if state.workers.isEmpty {
        state.reset(onCancel: { [weak context] generation in
          context?.withLock { $0.cancel(generation: generation) }
        })
      }

      let id = (state.workers.keys.max() ?? 0) + 1
      state.workers[id] = Task { [context] in
        await Self.workerLoop(id: id, context: context)
      }
    }
  }

  public func cancel() {
    context.withLock { $0.cancel(generation: nil) }
  }

  deinit {
    cancel()
  }
}

extension TaskWorklist/*: AsyncSequence*/ {
  public typealias AsyncIterator = AsyncStream<T>.AsyncIterator
  public typealias Element = T

  public var results: AsyncStream<T> {
    context.withLock(\.results)
  }

  public func makeAsyncIterator() -> AsyncStream<T>.AsyncIterator {
    results.makeAsyncIterator()
  }
}
