//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// .. Main Executor ............................................................

@available(SwiftStdlib 6.2, *)
final class UnimplementedMainExecutor: MainExecutor, @unchecked Sendable {
  public init() {}

  public func run() throws {
    fatalError("There is no executor implementation active")
  }

  public func stop() {
    fatalError("There is no executor implementation active")
  }

  #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  public func enqueue(_ job: UnownedJob) {
    fatalError("There is no executor implementation active")
  }
  #else
  public func enqueue(_ job: consuming ExecutorJob) {
    fatalError("There is no executor implementation active")
  }
  #endif

  public var isMainExecutor: Bool { true }

  public func checkIsolated() {
    // Do nothing
  }
}

// .. Task Executor ............................................................

@available(SwiftStdlib 6.2, *)
final class UnimplementedTaskExecutor: TaskExecutor, @unchecked Sendable {
  public init() {}

  #if SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  public func enqueue(_ job: UnownedJob) {
    fatalError("There is no executor implementation active")
  }
  #else
  public func enqueue(_ job: consuming ExecutorJob) {
    fatalError("There is no executor implementation active")
  }
  #endif

  public var isMainExecutor: Bool { false }
}
