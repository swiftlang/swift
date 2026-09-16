//===--- ChildTaskCreation.swift -----------------------------------------===//
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

// Compare the two structured-concurrency child-task shapes:
//   - `async let` (fixed arity, statically known at compile time)
//   - `withTaskGroup` (dynamic arity)
//
// Two group variants:
//   - one child (minimal group overhead)
//   - 100 children (steady-state group churn)

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  var b: [BenchmarkInfo] = [
    BenchmarkInfo(name: "ChildTaskCreation.AsyncLet.One",
                  runFunction: run_AsyncLet_One, tags: [.concurrency]),
    BenchmarkInfo(name: "ChildTaskCreation.AsyncLet.100",
                  runFunction: run_AsyncLet_100, tags: [.concurrency]),
    BenchmarkInfo(name: "ChildTaskCreation.TaskGroup.One",
                  runFunction: run_TaskGroup_One, tags: [.concurrency]),
    BenchmarkInfo(name: "ChildTaskCreation.TaskGroup.100",
                  runFunction: run_TaskGroup_100, tags: [.concurrency]),
  ]
  if #available(macOS 14, iOS 17, tvOS 17, watchOS 10, *) {
    b.append(BenchmarkInfo(name: "ChildTaskCreation.DiscardingTaskGroup.100",
                           runFunction: run_DiscardingTaskGroup_100, tags: [.concurrency]))
  }
  return b
}

@inline(never)
private func doWork() -> Int {
  var acc = 0
  for i in 0..<64 { acc = acc &+ i &* i }
  return acc
}

@inline(never)
public func run_AsyncLet_One(n: Int) async {
  for _ in 0..<n {
    async let child: Int = doWork()
    let value = await child
    blackHole(value)
  }
}

// 100 concurrent `async let` bindings in one lexical scope. `async let` is
// syntactically fixed-arity so we hand-write the 100 bindings and the 100
// awaits. This is what the user would write if they knew the fan-out at
// compile time.
@inline(never)
public func run_AsyncLet_100(n: Int) async {
  for _ in 0..<n {
    async let c0:  Int = doWork(); async let c1:  Int = doWork()
    async let c2:  Int = doWork(); async let c3:  Int = doWork()
    async let c4:  Int = doWork(); async let c5:  Int = doWork()
    async let c6:  Int = doWork(); async let c7:  Int = doWork()
    async let c8:  Int = doWork(); async let c9:  Int = doWork()
    async let c10: Int = doWork(); async let c11: Int = doWork()
    async let c12: Int = doWork(); async let c13: Int = doWork()
    async let c14: Int = doWork(); async let c15: Int = doWork()
    async let c16: Int = doWork(); async let c17: Int = doWork()
    async let c18: Int = doWork(); async let c19: Int = doWork()
    async let c20: Int = doWork(); async let c21: Int = doWork()
    async let c22: Int = doWork(); async let c23: Int = doWork()
    async let c24: Int = doWork(); async let c25: Int = doWork()
    async let c26: Int = doWork(); async let c27: Int = doWork()
    async let c28: Int = doWork(); async let c29: Int = doWork()
    async let c30: Int = doWork(); async let c31: Int = doWork()
    async let c32: Int = doWork(); async let c33: Int = doWork()
    async let c34: Int = doWork(); async let c35: Int = doWork()
    async let c36: Int = doWork(); async let c37: Int = doWork()
    async let c38: Int = doWork(); async let c39: Int = doWork()
    async let c40: Int = doWork(); async let c41: Int = doWork()
    async let c42: Int = doWork(); async let c43: Int = doWork()
    async let c44: Int = doWork(); async let c45: Int = doWork()
    async let c46: Int = doWork(); async let c47: Int = doWork()
    async let c48: Int = doWork(); async let c49: Int = doWork()
    async let c50: Int = doWork(); async let c51: Int = doWork()
    async let c52: Int = doWork(); async let c53: Int = doWork()
    async let c54: Int = doWork(); async let c55: Int = doWork()
    async let c56: Int = doWork(); async let c57: Int = doWork()
    async let c58: Int = doWork(); async let c59: Int = doWork()
    async let c60: Int = doWork(); async let c61: Int = doWork()
    async let c62: Int = doWork(); async let c63: Int = doWork()
    async let c64: Int = doWork(); async let c65: Int = doWork()
    async let c66: Int = doWork(); async let c67: Int = doWork()
    async let c68: Int = doWork(); async let c69: Int = doWork()
    async let c70: Int = doWork(); async let c71: Int = doWork()
    async let c72: Int = doWork(); async let c73: Int = doWork()
    async let c74: Int = doWork(); async let c75: Int = doWork()
    async let c76: Int = doWork(); async let c77: Int = doWork()
    async let c78: Int = doWork(); async let c79: Int = doWork()
    async let c80: Int = doWork(); async let c81: Int = doWork()
    async let c82: Int = doWork(); async let c83: Int = doWork()
    async let c84: Int = doWork(); async let c85: Int = doWork()
    async let c86: Int = doWork(); async let c87: Int = doWork()
    async let c88: Int = doWork(); async let c89: Int = doWork()
    async let c90: Int = doWork(); async let c91: Int = doWork()
    async let c92: Int = doWork(); async let c93: Int = doWork()
    async let c94: Int = doWork(); async let c95: Int = doWork()
    async let c96: Int = doWork(); async let c97: Int = doWork()
    async let c98: Int = doWork(); async let c99: Int = doWork()

    let sum =
      await c0  &+ c1  &+ c2  &+ c3  &+ c4  &+ c5  &+ c6  &+ c7  &+ c8  &+ c9  &+
      c10 &+ c11 &+ c12 &+ c13 &+ c14 &+ c15 &+ c16 &+ c17 &+ c18 &+ c19 &+
      c20 &+ c21 &+ c22 &+ c23 &+ c24 &+ c25 &+ c26 &+ c27 &+ c28 &+ c29 &+
      c30 &+ c31 &+ c32 &+ c33 &+ c34 &+ c35 &+ c36 &+ c37 &+ c38 &+ c39 &+
      c40 &+ c41 &+ c42 &+ c43 &+ c44 &+ c45 &+ c46 &+ c47 &+ c48 &+ c49 &+
      c50 &+ c51 &+ c52 &+ c53 &+ c54 &+ c55 &+ c56 &+ c57 &+ c58 &+ c59 &+
      c60 &+ c61 &+ c62 &+ c63 &+ c64 &+ c65 &+ c66 &+ c67 &+ c68 &+ c69 &+
      c70 &+ c71 &+ c72 &+ c73 &+ c74 &+ c75 &+ c76 &+ c77 &+ c78 &+ c79 &+
      c80 &+ c81 &+ c82 &+ c83 &+ c84 &+ c85 &+ c86 &+ c87 &+ c88 &+ c89 &+
      c90 &+ c91 &+ c92 &+ c93 &+ c94 &+ c95 &+ c96 &+ c97 &+ c98 &+ c99
    blackHole(sum)
  }
}

@inline(never)
public func run_TaskGroup_One(n: Int) async {
  for _ in 0..<n {
    let value = await withTaskGroup(of: Int.self) { group in
      group.addTask { doWork() }
      return await group.next() ?? 0
    }
    blackHole(value)
  }
}

@inline(never)
public func run_TaskGroup_100(n: Int) async {
  for _ in 0..<n {
    let sum = await withTaskGroup(of: Int.self) { group in
      for _ in 0..<100 {
        group.addTask { doWork() }
      }
      var acc = 0
      for await value in group {
        acc = acc &+ value
      }
      return acc
    }
    blackHole(sum)
  }
}

@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
@inline(never)
public func run_DiscardingTaskGroup_100(n: Int) async {
  for _ in 0..<n {
    await withDiscardingTaskGroup { group in
      for _ in 0..<100 {
        group.addTask {
          blackHole(doWork())
        }
      }
    }
  }
}
