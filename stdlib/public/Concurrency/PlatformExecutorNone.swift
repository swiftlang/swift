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

// .. Platform Main Executor ...................................................

/// `PlatformMainExecutor` is used during program start-up and also for any
/// `@MainActor` code.  It implements `SerialExecutor`, `RunLoopExecutor` and
/// `EventableExecutor`.
@available(SwiftStdlib 6.2, *)
public class PlatformMainExecutor: MainExecutor, @unchecked Sendable {

  public func run() throws {
    fatalError("There is no main executor implementation for this platform")
  }

  public func stop() {
    fatalError("There is no main executor implementation for this platform")
  }

  public func enqueue(_ job: consuming ExecutorJob) {
    fatalError("There is no main executor implementation for this platform")
  }

  public func registerEvent(handler: @escaping () -> ()) -> ExecutorEvent {
    fatalError("There is no main executor implementation for this platform")
  }

  public func deregister(event: ExecutorEvent) {
    fatalError("There is no main executor implementation for this platform")
  }

  public func notify(event: ExecutorEvent) {
    fatalError("There is no main executor implementation for this platform")
  }

}

// .. Platform Default Executor ................................................

/// `PlatformDefaultExecutor` is the default executor for non-`@MainActor`
/// tasks.  It implements `TaskExecutor` only.
@available(SwiftStdlib 6.2, *)
public class PlatformDefaultExecutor: TaskExecutor, @unchecked Sendable {

  public func enqueue(_ job: consuming ExecutorJob) {
    fatalError("There is no default executor implementation for this platform")
  }

}
