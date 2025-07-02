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

#if !$Embedded && !os(WASI)

import Swift

// We can't import Dispatch from here, sadly, because it apparently has a
// transitive dependency on Combine (which in turn depends on _Concurrency).

// import Dispatch

// .. Dispatch Interface .......................................................

// .. Main Executor ............................................................

/// A Dispatch-based main executor.
@available(StdlibDeploymentTarget 6.2, *)
class DispatchMainExecutor: RunLoopExecutor, SchedulingExecutor,
                            @unchecked Sendable {
  var threaded = false

  public init() {}

  public func run() throws {
    if threaded {
      fatalError("DispatchMainExecutor does not support recursion")
    }

    threaded = true
    _dispatchMain()
  }

  public func stop() {
    fatalError("DispatchMainExecutor cannot be stopped")
  }

  var asScheduling: (any SchedulingExecutor)? {
    return self
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    _dispatchEnqueue(job, at: instant, tolerance: tolerance, clock: clock,
                     executor: self, global: false)
  }
}

@available(StdlibDeploymentTarget 6.2, *)
extension DispatchMainExecutor: SerialExecutor {

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueMain(UnownedJob(job))
  }

  public var isMainExecutor: Bool { true }

  public func checkIsolated() {
    _dispatchAssertMainQueue()
  }
}

@available(StdlibDeploymentTarget 6.2, *)
extension DispatchMainExecutor: MainExecutor {}

// .. Task Executor ............................................................

/// A Dispatch-based `TaskExecutor`
@available(StdlibDeploymentTarget 6.2, *)
class DispatchGlobalTaskExecutor: TaskExecutor, SchedulingExecutor,
                                  @unchecked Sendable {
  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueGlobal(UnownedJob(job))
  }

  public var isMainExecutor: Bool { false }

  var asScheduling: (any SchedulingExecutor)? {
    return self
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    _dispatchEnqueue(job, at: instant, tolerance: tolerance, clock: clock,
                     executor: self, global: true)
  }
}

// .. Clock Support ............................................................

/// An enumeration identifying one of the Dispatch-supported clocks
enum DispatchClockID: CInt {
  case continuous = 1
  case suspending = 2
  case walltime = 3
}

fileprivate func clamp(_ components: (seconds: Int64, attoseconds: Int64))
  -> (seconds: Int64, attoseconds: Int64) {
  if components.seconds < 0
       || components.seconds == 0 && components.attoseconds < 0 {
    return (seconds: 0, attoseconds: 0)
  }
  return (seconds: components.seconds, attoseconds: components.attoseconds)
}

fileprivate func delay(from duration: Swift.Duration) -> (
  seconds: Int64, nanoseconds: Int64
) {
  let (seconds, attoseconds) = clamp(duration.components)
  let nanoseconds = attoseconds / 1_000_000_000
  return (seconds: seconds, nanoseconds: nanoseconds)
}

fileprivate func timestamp<C: Clock>(for instant: C.Instant, clock: C)
  -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64)? {
  if let continuousClock = clock as? ContinuousClock {
    return continuousClock.timestamp(for: instant as! ContinuousClock.Instant)
  } else if let suspendingClock = clock as? SuspendingClock {
    return suspendingClock.timestamp(for: instant as! SuspendingClock.Instant)
  }
  return nil
}

fileprivate func durationComponents<C: Clock>(for duration: C.Duration, clock: C)
  -> (seconds: Int64, nanoseconds: Int64) {
  if let continuousClock = clock as? ContinuousClock {
    return continuousClock.durationComponents(for: duration as! ContinuousClock.Duration)
  } else if let suspendingClock = clock as? SuspendingClock {
    return suspendingClock.durationComponents(for: duration as! SuspendingClock.Duration)
  }
  // This shouldn't be reachable
  fatalError("unknown clock in Dispatch Executor")
}

@available(StdlibDeploymentTarget 6.2, *)
fileprivate func _dispatchEnqueue<C: Clock, E: Executor>(
  _ job: consuming ExecutorJob,
  at instant: C.Instant,
  tolerance: C.Duration?,
  clock: C,
  executor: E,
  global: Bool
) {
  // If it's a clock we know, convert it to something we can use; otherwise,
  // call the clock's `enqueue` function to schedule the enqueue of the job.

  guard let (clockID, seconds, nanoseconds) = timestamp(for: instant,
                                                        clock: clock) else {
    clock.enqueue(job, on: executor, at: instant, tolerance: tolerance)
    return
  }

  let tolSec: Int64, tolNanosec: Int64
  if let tolerance = tolerance {
    (tolSec, tolNanosec) = durationComponents(for: tolerance,
                                              clock: clock)
  } else {
    tolSec = 0
    tolNanosec = -1
  }

  _dispatchEnqueueWithDeadline(CBool(global),
                               CLongLong(seconds), CLongLong(nanoseconds),
                               CLongLong(tolSec), CLongLong(tolNanosec),
                               clockID.rawValue,
                               UnownedJob(job))
}

#endif // !$Embedded && !os(WASI)
