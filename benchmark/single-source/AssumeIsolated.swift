//===--- AssumeIsolated.swift ---------------------------------------------===//
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
//
// Measures the per-call overhead of 'assumeIsolated'.
//
//===----------------------------------------------------------------------===//

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "AssumeIsolated.MainActor",
      runFunction: run_AssumeIsolatedMainActor,
      tags: [.concurrency],
      setUpFunction: setUp_AssumeIsolatedMainActor
    ),
    BenchmarkInfo(
      name: "AssumeIsolated.Actor",
      runFunction: run_AssumeIsolatedActor,
      tags: [.concurrency],
      setUpFunction: setUp_AssumeIsolatedActor
    ),
    // Same loop and body without assumeIsolated; the assumeIsolated cost is
    // the difference to the corresponding benchmark above
    BenchmarkInfo(
      name: "AssumeIsolated.MainActor.Baseline",
      runFunction: run_AssumeIsolatedMainActorBaseline,
      tags: [.concurrency],
      setUpFunction: setUp_AssumeIsolatedMainActor
    ),
    BenchmarkInfo(
      name: "AssumeIsolated.Actor.Baseline",
      runFunction: run_AssumeIsolatedActorBaseline,
      tags: [.concurrency],
      setUpFunction: setUp_AssumeIsolatedActor
    ),
  ]
}

let innerIterations = 1_000

// ==== -----------------------------------------------------------------------
// MARK: MainActor

// Not @MainActor so that setUp (synchronous, nonisolated) can reset it; the
// closures below are still main actor isolated through assumeIsolated's
// parameter type, and accessing the global costs the same either way
nonisolated(unsafe) private var mainActorCounter = 0

private func setUp_AssumeIsolatedMainActor() {
  mainActorCounter = 0
}

@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
@MainActor
@inline(never)
private func spinMainActor(_ n: Int) -> Int {
  for i in 0 ..< n {
    MainActor.assumeIsolated {
      mainActorCounter &+= identity(i)
    }
  }
  return mainActorCounter
}

@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
@MainActor
private func run_AssumeIsolatedMainActor(_ n: Int) async {
  blackHole(spinMainActor(n * innerIterations))
}

@MainActor
@inline(never)
private func spinMainActorBaseline(_ n: Int) -> Int {
  for i in 0 ..< n {
    mainActorCounter &+= identity(i)
  }
  return mainActorCounter
}

@MainActor
private func run_AssumeIsolatedMainActorBaseline(_ n: Int) async {
  blackHole(spinMainActorBaseline(n * innerIterations))
}

// ==== -----------------------------------------------------------------------
// MARK: Actor

private actor Counter {
  var value = 0

  @available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
  @inline(never)
  func spin(_ n: Int) -> Int {
    for i in 0 ..< n {
      self.assumeIsolated { counter in
        counter.value &+= identity(i)
      }
    }
    return value
  }

  @inline(never)
  func spinBaseline(_ n: Int) -> Int {
    for i in 0 ..< n {
      value &+= identity(i)
    }
    return value
  }
}

private let counter = Counter()
private func setUp_AssumeIsolatedActor() {
  blackHole(counter)
}

@available(macOS 14.0, iOS 17.0, tvOS 17.0, watchOS 10.0, *)
private func run_AssumeIsolatedActor(_ n: Int) async {
  let res = await counter.spin(n * innerIterations)
  blackHole(res)
}

private func run_AssumeIsolatedActorBaseline(_ n: Int) async {
  let res = await counter.spinBaseline(n * innerIterations)
  blackHole(res)
}
