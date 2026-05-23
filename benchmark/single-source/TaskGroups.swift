//===--- TaskGroup.swift --------------------------------------------------===//
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

public var benchmarks: [BenchmarkInfo] {
  guard #available(macOS 14, iOS 17, tvOS 17, watchOS 10, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "TaskGroups.1000",
      runFunction: run_TaskGroup(groupSize: 1000),
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskGroups.5000",
      runFunction: run_TaskGroup(groupSize: 5000),
      tags: [.concurrency]
    )
  ]
}

@available(macOS 14, iOS 17, tvOS 17, watchOS 10, *)
private func run_TaskGroup(groupSize: Int) -> (Int) -> Void {
  return { n in
    let g = DispatchGroup()
    g.enter()
    Task {
      for _ in 0..<n {
        var task: Task<Void, Never>! = nil

        let cont: UnsafeContinuation<Void, Never> = await withUnsafeContinuation { cont in
          task = Task {
            await withUnsafeContinuation {
              cont.resume(returning: $0)
            }
          }
        }

        await withDiscardingTaskGroup(returning: Void.self) { group in
          for i in 0..<groupSize {
            group.addTask {
              await task.value
            }
          }
          cont.resume()
        }
      }
      g.leave()
    }
    g.wait()
  }
}
