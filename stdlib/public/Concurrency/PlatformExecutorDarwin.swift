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

#if !$Embedded && (os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS))

import Swift

// .. Platform Main Executor ...................................................

/// `PlatformMainExecutor` is used during program start-up and also for any
/// `@MainActor` code.  It implements `SerialExecutor`, `RunLoopExecutor` and
/// `EventableExecutor`.
@available(SwiftStdlib 6.2, *)
public class PlatformMainExecutor: MainExecutor, @unchecked Sendable {
  var executor: any MainExecutor

  init() {
    if CoreFoundation.isPresent {
      executor = CFMainExecutor()
    } else {
      executor = DispatchMainExecutor()
    }
  }

  public var supportsScheduling: Bool { executor.supportsScheduling }

  public func enqueue(_ job: consuming ExecutorJob) {
    executor.enqueue(job)
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    executor.enqueue(job, after: delay, tolerance: tolerance, clock: clock)
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    executor.enqueue(job, at: instant, tolerance: tolerance, clock: clock)
  }

  public func run() throws {
    try executor.run()
  }

  public func run(until condition: () -> Bool) throws {
    try executor.run(until: condition)
  }

  public func stop() {
    executor.stop()
  }

  public func registerEvent(handler: @escaping () -> ()) -> ExecutorEvent {
    return executor.registerEvent(handler: handler)
  }

  public func deregister(event: ExecutorEvent) {
    executor.deregister(event: event)
  }

  public func notify(event: ExecutorEvent) {
    executor.notify(event: event)
  }

  public func checkIsolated() {
    executor.checkIsolated()
  }
}

// .. Platform Task Executor ...................................................

/// `PlatformDefaultExecutor` is the default executor for non-`@MainActor`
/// tasks.  It implements `TaskExecutor` only.
@available(SwiftStdlib 6.2, *)
public class PlatformDefaultExecutor: TaskExecutor, @unchecked Sendable {
  var executor: any TaskExecutor

  init() {
    if CoreFoundation.isPresent {
      executor = CFTaskExecutor()
    } else {
      executor = DispatchTaskExecutor()
    }
  }

  public var isMainExecutor: Bool { executor.isMainExecutor }

  public var supportsScheduling: Bool { executor.supportsScheduling }

  public func enqueue(_ job: consuming ExecutorJob) {
    executor.enqueue(job)
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                after delay: C.Duration,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    executor.enqueue(job, after: delay, tolerance: tolerance, clock: clock)
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    executor.enqueue(job, at: instant, tolerance: tolerance, clock: clock)
  }
}

#endif // os(macOS) || os(iOS) || os(tvOS) || os(watchOS) || os(visionOS)
