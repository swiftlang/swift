//===--- TaskIsCancelledCancellationScopes.swift --------------------------===//
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

// Measure `Task.isCancelled` under various shield/scope
// nesting shapes. Each variant sets up a specific record-chain state,
// then runs a hot inner loop of `Task.isCancelled` reads.
//
// Expected outcomes qualitatively:
//   - FastPath / ShieldOnly: no scope on chain -> bit-only fast path, cheapest
//   - InScope*: scope on chain -> walker takes the record lock every read
//   - InnerShield vs OuterShield: same walker cost + one shield-record hit

@_spi(Concurrency) import _Concurrency
import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  // These benchmarks exercise APIs currently only present on the
  // just-built toolchain / stdlib. On any shipping OS the `#available`
  // check below fails and the benchmark list is empty.
  guard #available(macOS 9999, *) else { return [] }
  return [
    BenchmarkInfo(name: "TaskIsCancelled.FastPath",
                  runFunction: run_FastPath, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskIsCancelled.ShieldOnly",
                  runFunction: run_ShieldOnly, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskIsCancelled.InScopeUncancelled",
                  runFunction: run_InScopeUncancelled, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskIsCancelled.InScopeCancelled",
                  runFunction: run_InScopeCancelled, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskIsCancelled.InScopeInnerShield",
                  runFunction: run_InScopeInnerShield, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskIsCancelled.InScopeOuterShield",
                  runFunction: run_InScopeOuterShield, tags: [.concurrency]),
  ]
}

@inline(never)
private func hotLoop(_ n: Int) -> Int {
  var trueCount = 0
  for _ in 0..<n {
    if Task.isCancelled { trueCount &+= 1 }
  }
  return trueCount
}

@available(macOS 9999, *)
@inline(never)
public func run_FastPath(n: Int) async {
  // No scope, no shield: pure bit check.
  blackHole(hotLoop(n * 1000))
}

@available(macOS 9999, *)
@inline(never)
public func run_ShieldOnly(n: Int) async {
  await withTaskCancellationShield {
    // Shield bit set, no scope: bit check + shield mask, no walk.
    blackHole(hotLoop(n * 1000))
  }
}

@available(macOS 9999, *)
@inline(never)
public func run_InScopeUncancelled(n: Int) async {
  await __withTaskCancellationScope { _ in
    // Scope on chain but not cancelled: walker runs, returns nullptr.
    blackHole(hotLoop(n * 1000))
  }
}

@available(macOS 9999, *)
@inline(never)
public func run_InScopeCancelled(n: Int) async {
  await __withTaskCancellationScope { scope in
    scope.cancel()
    // Scope cancelled: walker returns the scope record.
    blackHole(hotLoop(n * 1000))
  }
}

@available(macOS 9999, *)
@inline(never)
public func run_InScopeInnerShield(n: Int) async {
  await __withTaskCancellationScope { scope in
    scope.cancel()
    await withTaskCancellationShield {
      // Innermost record above the cancelled scope is a shield: walker
      // sees SHIELD first and short-circuits. Also exercises the shield
      // push/pop for a nested-inside-scope shield.
      blackHole(hotLoop(n * 1000))
    }
  }
}

@available(macOS 9999, *)
@inline(never)
public func run_InScopeOuterShield(n: Int) async {
  await withTaskCancellationShield {
    await __withTaskCancellationScope { scope in
      scope.cancel()
      // Shield is OUTSIDE the scope on the chain: walker sees SCOPE
      // (cancelled) first, returns true. Positional walk still runs.
      blackHole(hotLoop(n * 1000))
    }
  }
}
