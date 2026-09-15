//===--- AsyncLoops.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Loops whose body awaits something that usually resumes synchronously, i.e.
// without going back through an executor. On targets where async resumption
// is a guaranteed tail call these measure the plain cost of such an await;
// on targets where it is not (WebAssembly without the tail-call feature) the
// compiler inserts an executor yield on every iteration of these loops to
// bound native stack usage, and these benchmarks measure that yield.

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 12, iOS 15, tvOS 15, watchOS 8, *) else {
    return []
  }
  return [
    BenchmarkInfo(name: "AsyncLoop.SyncAwait",
                  runFunction: run_SyncAwait, tags: [.concurrency]),
    BenchmarkInfo(name: "AsyncLoop.ImmediateContinuation",
                  runFunction: run_ImmediateContinuation, tags: [.concurrency]),
    BenchmarkInfo(name: "AsyncLoop.SyncAsyncSequence",
                  runFunction: run_SyncAsyncSequence, tags: [.concurrency]),
    BenchmarkInfo(name: "AsyncLoop.AsyncStream",
                  runFunction: run_AsyncStream, tags: [.concurrency]),
    BenchmarkInfo(name: "AsyncLoop.ActorHop",
                  runFunction: run_ActorHop, tags: [.concurrency]),
  ]
}

private let loopLength = 1_000

/// Completes without suspending, so the caller resumes synchronously.
@inline(never)
private func compute(_ x: Int) async -> Int {
  return x &* 3
}

/// A loop of awaits on a synchronously completing async function.
@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
public func run_SyncAwait(n: Int) async {
  var acc = 0
  for _ in 0..<n {
    for i in 0..<loopLength {
      acc &+= await compute(i)
    }
  }
  check(acc == n * (3 * loopLength * (loopLength - 1) / 2))
}

/// A loop of continuations that are resumed before they are awaited.
@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
public func run_ImmediateContinuation(n: Int) async {
  var acc = 0
  for _ in 0..<n {
    for i in 0..<loopLength {
      acc &+= await withUnsafeContinuation { $0.resume(returning: i) }
    }
  }
  check(acc == n * (loopLength * (loopLength - 1) / 2))
}

/// An AsyncSequence whose iterator never suspends.
@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
private struct Counting: AsyncSequence {
  typealias Element = Int
  let count: Int

  struct AsyncIterator: AsyncIteratorProtocol {
    var next_ = 0
    let count: Int

    mutating func next() async -> Int? {
      guard next_ < count else { return nil }
      defer { next_ += 1 }
      return next_
    }
  }

  func makeAsyncIterator() -> AsyncIterator {
    return AsyncIterator(count: count)
  }
}

/// A `for await` loop over a synchronous AsyncSequence through the stdlib's
/// map and filter combinators.
@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
public func run_SyncAsyncSequence(n: Int) async {
  var acc = 0
  for _ in 0..<n {
    for await x in Counting(count: loopLength)
                     .map({ $0 &* 2 })
                     .filter({ $0 % 4 == 0 }) {
      acc &+= x
    }
  }
  blackHole(acc)
}

/// A `for await` loop over an AsyncStream whose elements are all buffered
/// before the loop starts, so `next()` resumes synchronously.
@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
public func run_AsyncStream(n: Int) async {
  var acc = 0
  for _ in 0..<n {
    let stream = AsyncStream<Int> { continuation in
      for i in 0..<loopLength {
        continuation.yield(i)
      }
      continuation.finish()
    }
    for await x in stream {
      acc &+= x
    }
  }
  check(acc == n * (loopLength * (loopLength - 1) / 2))
}

@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
private actor Counter {
  var value = 0

  func add(_ x: Int) -> Int {
    value &+= x
    return value
  }
}

/// A loop of calls onto an actor from outside it, i.e. two executor switches
/// per iteration, for comparison with the synchronously resuming loops.
@available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
public func run_ActorHop(n: Int) async {
  let counter = Counter()
  var acc = 0
  for _ in 0..<n {
    for i in 0..<loopLength {
      acc &+= await counter.add(i)
    }
  }
  blackHole(acc)
}
