//===--- ClockMeasure.swift -----------------------------------------------===//
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

// Clock.measure runs in the _Concurrency library, unspecialized for the
// concrete clock.

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 13, iOS 16, tvOS 16, watchOS 9, *) else {
    return []
  }
  return [
    BenchmarkInfo(name: "ClockMeasure.Continuous",
                  runFunction: run_ClockMeasureContinuous,
                  tags: [.concurrency]),
    BenchmarkInfo(name: "ClockMeasure.Suspending",
                  runFunction: run_ClockMeasureSuspending,
                  tags: [.concurrency]),
  ]
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func measureEmpty<C: Clock>(_ clock: C, _ n: Int) -> C.Duration {
  var total = C.Duration.zero
  for _ in 0..<n {
    for _ in 0..<10_000 {
      total += clock.measure {}
    }
  }
  return total
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockMeasureContinuous(_ n: Int) {
  blackHole(measureEmpty(ContinuousClock(), n))
}

@available(macOS 13, iOS 16, tvOS 16, watchOS 9, *)
@inline(never)
private func run_ClockMeasureSuspending(_ n: Int) {
  blackHole(measureEmpty(SuspendingClock(), n))
}
