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
public class DummyMainExecutor: MainExecutor, @unchecked Sendable {
  public init() {}

  public func run() throws {
    fatalError("There is no executor implementation active")
  }

  public func stop() {
    fatalError("There is no executor implementation active")
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    fatalError("There is no executor implementation active")
  }

  public var isMainExecutor: Bool { true }

  public func checkIsolated() {
    // Do nothing
  }
}

// .. Task Executor ............................................................

@available(SwiftStdlib 6.2, *)
public class DummyTaskExecutor: TaskExecutor, @unchecked Sendable {
  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    fatalError("There is no executor implementation active")
  }

  public var isMainExecutor: Bool { false }
}
