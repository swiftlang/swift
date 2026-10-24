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

#if !$Embedded && !os(WASI) && !os(Emscripten)

import Swift

// We can't import Dispatch from here, sadly, because it apparently has a
// transitive dependency on Combine (which in turn depends on _Concurrency).

// import Dispatch

// .. Dispatch Interface .......................................................

// .. Main Executor ............................................................

/// A Dispatch-based main executor.
@available(StdlibDeploymentTarget 6.3, *)
class DispatchMainExecutor: RunLoopExecutor, @unchecked Sendable {
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
}

@available(StdlibDeploymentTarget 6.3, *)
extension DispatchMainExecutor: SerialExecutor {

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueMain(UnownedJob(job))
  }

  public func checkIsolated() {
    _dispatchAssertMainQueue()
  }
}

@available(StdlibDeploymentTarget 6.3, *)
extension DispatchMainExecutor: MainExecutor {}

@available(StdlibDeploymentTarget 6.5, *)
extension DispatchMainExecutor: SchedulingExecutor {
  public func schedule<C: Clock>(_ job: consuming ExecutorJob,
                                 at fireTime: SchedulingExecutorFireTime<C>,
                                 tolerance: C.Duration?,
                                 clock: C) -> JobCancellationToken {
    return _dispatchEnqueue(job, run: fireTime, tolerance: tolerance,
                            clock: clock, executor: self, global: false)
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    return _dispatchCancel(token.opaqueData[0])
  }

  public func supportsScheduling(on clock: SystemClockID) -> Bool {
    return true
  }

  public func release(token: borrowing JobCancellationToken) {
    _dispatchReleaseSource(token.opaqueData[0])
  }
}

// .. Task Executor ............................................................

/// A Dispatch-based `TaskExecutor`
@available(StdlibDeploymentTarget 6.3, *)
class DispatchGlobalTaskExecutor: TaskExecutor, @unchecked Sendable {
  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueGlobal(UnownedJob(job))
  }
}
@available(StdlibDeploymentTarget 6.5, *)
extension DispatchGlobalTaskExecutor: SchedulingExecutor {
  public func schedule<C: Clock>(_ job: consuming ExecutorJob,
                                 at fireTime: SchedulingExecutorFireTime<C>,
                                 tolerance: C.Duration?,
                                 clock: C) -> JobCancellationToken {
    return _dispatchEnqueue(job, run: fireTime, tolerance: tolerance,
                            clock: clock, executor: self, global: true)
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken)
    -> ExecutorJob? {
    return _dispatchCancel(token.opaqueData[0])
  }

  public func supportsScheduling(on clock: SystemClockID) -> Bool {
    return true
  }

  public func release(token: borrowing JobCancellationToken) {
    _dispatchReleaseSource(token.opaqueData[0])
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

/// Returns `true` if `clock` is one of the clocks that we schedule ourselves.
fileprivate func _isDispatchClock<C: Clock>(_ clock: C) -> Bool {
  return clock is ContinuousClock || clock is SuspendingClock
}

/// The arguments that `swift_dispatchEnqueueWithDeadline` and its
/// continuation counterpart take in place of a fire time and tolerance.
@available(StdlibDeploymentTarget 6.5, *)
fileprivate func _deadlineArguments<C: Clock>(
  run: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?,
  clock: C
) -> (clockID: CInt, seconds: CLongLong, nanoseconds: CLongLong,
      tolSec: CLongLong, tolNanosec: CLongLong) {
  // If it's a clock we know, convert it to something we can use; otherwise,
  // fail.
  let instant = run.asInstant(clock: clock)

  guard let (clockID, seconds, nanoseconds) = timestamp(for: instant,
                                                        clock: clock) else {
    fatalError("Sorry, cannot schedule on an unknown clock")
  }

  let tolSec: Int64, tolNanosec: Int64
  if let tolerance = tolerance {
    (tolSec, tolNanosec) = durationComponents(for: tolerance,
                                              clock: clock)
  } else {
    tolSec = 0
    tolNanosec = -1
  }

  return (clockID: clockID.rawValue,
          seconds: CLongLong(seconds), nanoseconds: CLongLong(nanoseconds),
          tolSec: CLongLong(tolSec), tolNanosec: CLongLong(tolNanosec))
}

@available(StdlibDeploymentTarget 6.5, *)
fileprivate func _dispatchToken<E: SchedulingExecutor>(
  executor: E, source: Int
) -> JobCancellationToken {
  return unsafe JobCancellationToken(canceller: executor,
                                     opaqueData: [source, 0])
}

@available(StdlibDeploymentTarget 6.5, *)
fileprivate func _dispatchEnqueue<C: Clock, E: SchedulingExecutor>(
  _ job: consuming ExecutorJob,
  run: SchedulingExecutorFireTime<C>,
  tolerance: C.Duration?,
  clock: C,
  executor: E,
  global: Bool
) -> JobCancellationToken {
  // A clock we don't recognise may know how to schedule the job itself.
  if !_isDispatchClock(clock) && _isSchedulingClock(clock) {
    return _scheduleUsingClock(job, on: executor, at: run,
                              tolerance: tolerance, clock: clock)
  }

  let args = _deadlineArguments(run: run, tolerance: tolerance, clock: clock)
  let source = _dispatchEnqueueWithDeadline(CBool(global),
                                            args.seconds, args.nanoseconds,
                                            args.tolSec, args.tolNanosec,
                                            args.clockID,
                                            UnownedJob(job))
  return _dispatchToken(executor: executor, source: source)
}

#endif // !$Embedded && !os(WASI) && !os(Emscripten)
