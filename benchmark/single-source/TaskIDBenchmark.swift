//===--- TaskID.swift -----------------------------------------------------===//
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

// Measures the cost of reading the current Task's ID.

import TestsUtils

public var benchmarks: [BenchmarkInfo] {
  guard #available(anyAppleOS 9999, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "TaskID.currentID",
      runFunction: run_TaskID_currentID,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskID.unsafeCurrent.id",
      runFunction: run_TaskID_withUnsafeCurrentTask,
      tags: [.concurrency]
    ),
  ]
}

// Direct accessor: single runtime call, no closure, no ARC on the task.
@available(anyAppleOS 9999, *)
private func run_TaskID_currentID(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<80_000 {
      blackHole(Task.currentID)
    }
  }
}

// Equivalent shape built on withUnsafeCurrentTask.
@available(anyAppleOS 9999, *)
private func run_TaskID_withUnsafeCurrentTask(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<32_000 {
      withUnsafeCurrentTask { task in
        blackHole(task?.id)
      }
    }
  }
}
