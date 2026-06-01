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
  guard #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) else {
    return []
  }
  return [
    BenchmarkInfo(
      name: "TaskID.currentID",
      runFunction: run_TaskID_currentID,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskID.withUnsafeCurrentTask.id",
      runFunction: run_TaskID_withUnsafeCurrentTask,
      tags: [.concurrency]
    ),
    BenchmarkInfo(
      name: "TaskID.directBuiltins",
      runFunction: run_TaskID_directBuiltins,
      tags: [.concurrency]
    ),
  ]
}

// Direct accessor: single runtime call, no closure, no ARC on the task.
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
private func run_TaskID_currentID(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      blackHole(Task.currentID)
    }
  }
}

// Equivalent shape built on withUnsafeCurrentTask.
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
private func run_TaskID_withUnsafeCurrentTask(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      withUnsafeCurrentTask { task in
        blackHole(task?.id)
      }
    }
  }
}

#if canImport(Darwin)
import Darwin

private typealias _GetCurrentTaskFn = @convention(c) () -> OpaquePointer?
private typealias _GetJobTaskIdFn = @convention(c) (OpaquePointer) -> UInt64

private let _bench_swift_task_getCurrent: _GetCurrentTaskFn = unsafeBitCast(
  dlsym(UnsafeMutableRawPointer(bitPattern: -2), "swift_task_getCurrent")!,
  to: _GetCurrentTaskFn.self
)
private let _bench_swift_task_getJobTaskId: _GetJobTaskIdFn = unsafeBitCast(
  dlsym(UnsafeMutableRawPointer(bitPattern: -2), "swift_task_getJobTaskId")!,
  to: _GetJobTaskIdFn.self
)

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
private func run_TaskID_directBuiltins(_ n: Int) async {
  for _ in 0..<n {
    for _ in 0..<10_000 {
      if let task = _bench_swift_task_getCurrent() {
        blackHole(_bench_swift_task_getJobTaskId(task))
      } else {
        blackHole(UInt64(0))
      }
    }
  }
}
#else
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
private func run_TaskID_directBuiltins(_ n: Int) async {}
#endif
