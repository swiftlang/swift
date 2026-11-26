//===--- TaskCancellation.swift -------------------------------------------===//
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

public var benchmarks: [BenchmarkInfo] {
  guard #available(anyAppleOS 9999, *) else { return [] }
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
    BenchmarkInfo(name: "TaskCancellation.ScopeEnter",
                  runFunction: run_ScopeEnter, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskCancellation.ShieldEnter",
                  runFunction: run_ShieldEnter, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskCancellation.ScopeEnterNested4",
                  runFunction: run_ScopeEnterNested4, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskCancellation.ScopeCancel",
                  runFunction: run_ScopeCancel, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskCancellation.ScopeCancelWithHandler",
                  runFunction: run_ScopeCancelWithHandler, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskCancellation.ScopeReadState",
                  runFunction: run_ScopeReadState, tags: [.concurrency]),
    BenchmarkInfo(name: "TaskCancellation.TaskGroupCancel",
                  runFunction: run_TaskGroupCancel, tags: [.concurrency]),
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

// Makes a closure async, so that it uses the async overload of a scope or
// shield.
@inline(never)
private func asyncWork() async -> Int { 1 }

// MARK: Task.isCancelled

// Measure `Task.isCancelled` under various shield/scope
// nesting shapes. Each variant sets up a specific record-chain state,
// then runs a hot inner loop of `Task.isCancelled` reads.

@available(anyAppleOS 9999, *)
@inline(never)
public func run_FastPath(n: Int) async {
  // No scope, no shield: pure bit check.
  blackHole(hotLoop(n * 1000))
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_ShieldOnly(n: Int) async {
  await withTaskCancellationShield {
    // Shield bit set, no scope: bit check + shield mask, no walk.
    blackHole(hotLoop(n * 1000) &+ (await asyncWork()))
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_InScopeUncancelled(n: Int) async {
  await withTaskCancellationScope { _ in
    // Scope on chain but not cancelled: walker runs, returns nullptr.
    blackHole(hotLoop(n * 1000) &+ (await asyncWork()))
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_InScopeCancelled(n: Int) async {
  await withTaskCancellationScope { scope in
    scope.cancel()
    // Scope cancelled: walker returns the scope record.
    blackHole(hotLoop(n * 1000) &+ (await asyncWork()))
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_InScopeInnerShield(n: Int) async {
  await withTaskCancellationScope { scope in
    scope.cancel()
    await withTaskCancellationShield {
      // Innermost record above the cancelled scope is a shield: walker
      // sees SHIELD first and short-circuits. Also exercises the shield
      // push/pop for a nested-inside-scope shield.
      blackHole(hotLoop(n * 1000) &+ (await asyncWork()))
    }
  }
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_InScopeOuterShield(n: Int) async {
  await withTaskCancellationShield {
    await withTaskCancellationScope { scope in
      scope.cancel()
      // Shield is OUTSIDE the scope: walker sees SCOPE (cancelled) first,
      // returns true. Positional walk still runs.
      blackHole(hotLoop(n * 1000) &+ (await asyncWork()))
    }
  }
}

// MARK: Cancellation scopes

@available(anyAppleOS 9999, *)
@inline(never)
public func run_ScopeEnter(n: Int) async {
  var acc = 0
  for _ in 0..<(n * 1000) {
    acc &+= await withTaskCancellationScope { _ in await asyncWork() }
  }
  blackHole(acc)
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_ScopeEnterNested4(n: Int) async {
  var acc = 0
  for _ in 0..<(n * 250) {
    acc &+= await withTaskCancellationScope { _ in
      await withTaskCancellationScope { _ in
        await withTaskCancellationScope { _ in
          await withTaskCancellationScope { _ in await asyncWork() }
        }
      }
    }
  }
  blackHole(acc)
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_ScopeCancel(n: Int) async {
  var acc = 0
  for _ in 0..<(n * 1000) {
    acc &+= await withTaskCancellationScope { scope in
      scope.cancel()
      return Task.isCancelled ? await asyncWork() : 0
    }
  }
  blackHole(acc)
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_ScopeCancelWithHandler(n: Int) async {
  var acc = 0
  for _ in 0..<(n * 1000) {
    acc &+= await withTaskCancellationScope { scope in
      await withTaskCancellationHandler {
        scope.cancel()
        return await asyncWork()
      } onCancel: {
        blackHole(1)
      }
    }
  }
  blackHole(acc)
}

@available(anyAppleOS 9999, *)
@inline(never)
public func run_ScopeReadState(n: Int) async {
  await withTaskCancellationScope { scope in
    scope.cancel(reason: .deadlineExpired)
    var acc = 0
    for _ in 0..<(n * 1000) {
      if scope.isCancelled { acc &+= 1 }
      if scope.cancellationReason != nil { acc &+= 1 }
    }
    blackHole(acc)
    _ = await asyncWork()
  }
}

// MARK: Cancellation shields

@available(anyAppleOS 9999, *)
@inline(never)
public func run_ShieldEnter(n: Int) async {
  var acc = 0
  for _ in 0..<(n * 1000) {
    acc &+= await withTaskCancellationShield { await asyncWork() }
  }
  blackHole(acc)
}

// MARK: Task groups

// The same work as `run_ScopeCancel`, with a child task as the cancellation
// boundary.
@available(anyAppleOS 9999, *)
@inline(never)
public func run_TaskGroupCancel(n: Int) async {
  var acc = 0
  for _ in 0..<(n * 1000) {
    acc &+= await withTaskGroup(of: Int.self) { group in
      group.addTask { Task.isCancelled ? await asyncWork() : 0 }
      group.cancelAll()
      return await group.next() ?? 0
    }
  }
  blackHole(acc)
}
