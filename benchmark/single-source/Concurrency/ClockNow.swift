//===--- ClockNow.swift ---------------------------------------------------===//
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

// The clock entry points of the Concurrency runtime: `now` and
// `minimumResolution` of the standard clocks, and zero-length sleeps.

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 13, iOS 16, tvOS 16, watchOS 9, *) else {
    return []
  }
  return [
    BenchmarkInfo(name: "ClockNow.Continuous",
                  runFunction: run_ClockNowContinuous,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "ClockNow.Suspending",
                  runFunction: run_ClockNowSuspending,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "ClockNow.ContinuousResolution",
                  runFunction: run_ClockNowContinuousResolution,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "ClockNow.SuspendingResolution",
                  runFunction: run_ClockNowSuspendingResolution,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "ClockNow.InstantArithmetic",
                  runFunction: run_ClockNowInstantArithmetic,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "ClockNow.SleepZero",
                  runFunction: run_ClockNowSleepZero,
                  tags: [.concurrency]),
  ]
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockNowContinuous(_ n: Int) {
  var last = ContinuousClock.now
  for _ in 0..<(n * 10_000) {
    last = max(last, ContinuousClock.now)
  }
  blackHole(last)
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockNowSuspending(_ n: Int) {
  var last = SuspendingClock.now
  for _ in 0..<(n * 10_000) {
    last = max(last, SuspendingClock.now)
  }
  blackHole(last)
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockNowContinuousResolution(_ n: Int) {
  let clock = identity(ContinuousClock())
  var total = Duration.zero
  for _ in 0..<(n * 10_000) {
    total += clock.minimumResolution
  }
  blackHole(total)
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockNowSuspendingResolution(_ n: Int) {
  let clock = identity(SuspendingClock())
  var total = Duration.zero
  for _ in 0..<(n * 10_000) {
    total += clock.minimumResolution
  }
  blackHole(total)
}

// A deadline from `now`, and the time left until it.
@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockNowInstantArithmetic(_ n: Int) {
  let delay = identity(Duration.milliseconds(5))
  var total = Duration.zero
  for _ in 0..<(n * 10_000) {
    let deadline = ContinuousClock.now.advanced(by: delay)
    total += ContinuousClock.now.duration(to: deadline)
  }
  blackHole(total)
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockNowSleepZero(_ n: Int) async {
  for _ in 0..<(n * 100) {
    try? await Task.sleep(for: .zero)
  }
}
