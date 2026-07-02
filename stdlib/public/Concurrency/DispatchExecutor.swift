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

fileprivate let opaqueDataDispatchSource = 0

// .. Dispatch Interface .......................................................

// .. Main Executor ............................................................

/// A Dispatch-based main executor.
@available(StdlibDeploymentTarget 9999, *)
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

  public func enqueue<C: Clock>(
    _ job: consuming ExecutorJob,
    run at: FireTime<C>,
    clock: C,
    tolerance: C.Duration?,
    onCancellation behavior: CancellationBehavior
  ) -> JobCancellationToken {
    return _dispatchEnqueue(
      job, run: at, tolerance: tolerance, clock: clock,
      executor: self, onCancellation: behavior, global: false
    )
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken) {
    if let job = _dispatchCancel(token.opaqueData[opaqueDataDispatchSource]) {
      switch token.cancellationBehavior {
        case .drop:
          // Destroy the job
          job.destroy()

        case .executeImmediately:
          // Need to immediately enqueue the job
          enqueue(job)
      }
    }
  }

  @available(StdlibDeploymentTarget 9999, *)
  internal var asSchedulingExecutor: (any SchedulingExecutor)? {
    return self
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension DispatchMainExecutor: SerialExecutor {

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueMain(UnownedJob(job))
  }

  public func checkIsolated() {
    _dispatchAssertMainQueue()
  }
}

@available(StdlibDeploymentTarget 9999, *)
extension DispatchMainExecutor: MainExecutor {}

// .. Task Executor ............................................................

/// A Dispatch-based `TaskExecutor`
@available(StdlibDeploymentTarget 9999, *)
class DispatchGlobalTaskExecutor: TaskExecutor, SchedulingExecutor,
                                  @unchecked Sendable {
  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueGlobal(UnownedJob(job))
  }

  public func enqueue<C: Clock>(
    _ job: consuming ExecutorJob,
    run at: FireTime<C>,
    clock: C,
    tolerance: C.Duration?,
    onCancellation behavior: CancellationBehavior
  ) -> JobCancellationToken {
    return _dispatchEnqueue(
      job, run: at, tolerance: tolerance, clock: clock,
      executor: self, onCancellation: behavior, global: true
    )
  }

  public func cancel(jobWithToken token: consuming JobCancellationToken) {
    if let job = _dispatchCancel(token.opaqueData[opaqueDataDispatchSource]) {
      switch token.cancellationBehavior {
        case .drop:
          // Destroy the job
          job.destroy()

        case .executeImmediately:
          // Need to immediately enqueue the job
          enqueue(job)
      }
    }
  }

  @available(StdlibDeploymentTarget 9999, *)
  internal var asSchedulingExecutor: (any SchedulingExecutor)? {
    return self
  }
}

// .. Clock Support ............................................................

/// An enumeration identifying one of the Dispatch-supported clocks
enum DispatchClockID: CInt {
  case continuous = 1
  case suspending = 2
  case walltime = 3
}

@_unavailableInEmbedded
extension ContinuousClock {
  func timestamp(for instant: Instant)
    -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, nanoseconds) = durationComponents(for: instant._value)
    return (clockID: .continuous, seconds: seconds, nanoseconds: nanoseconds)
  }

  func durationComponents(for duration: Duration)
    -> (seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000
    return (seconds: seconds, nanoseconds: nanoseconds)
  }
}

@_unavailableInEmbedded
extension SuspendingClock {
  func timestamp(for instant: Instant)
    -> (clockID: _ClockID, seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, nanoseconds) = durationComponents(for: instant._value)
    return (clockID: .suspending, seconds: seconds, nanoseconds: nanoseconds)
  }

  func durationComponents(for duration: Duration)
    -> (seconds: Int64, nanoseconds: Int64)
  {
    let (seconds, attoseconds) = duration.components
    let nanoseconds = attoseconds / 1_000_000_000
    return (seconds: seconds, nanoseconds: nanoseconds)
  }
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

@available(StdlibDeploymentTarget 9999, *)
fileprivate func _dispatchEnqueue<C: Clock, E: SchedulingExecutor>(
  _ job: consuming ExecutorJob,
  run at: FireTime<C>,
  tolerance: C.Duration?,
  clock: C,
  executor: E,
  onCancellation behavior: CancellationBehavior,
  global: Bool
) -> JobCancellationToken {
  // If it's a clock we know, convert it to something we can use; otherwise,
  // call the clock's `enqueue` function to schedule the enqueue of the job.
  let instant = at.asInstant(clock: clock)

  guard let (clockID, seconds, nanoseconds) = timestamp(for: instant,
                                                        clock: clock) else {
    if let enqueueingClock = clock as? any EnqueueingClock {
      return executor._openAndEnqueue(
        job,
        run: at,
        tolerance: tolerance,
        clock: clock,
        onCancellation: behavior,
        enqueueingClock: enqueueingClock
      )
    } else {
      fatalError("Sorry, cannot schedule on an unknown clock")
    }
  }

  let tolSec: Int64, tolNanosec: Int64
  if let tolerance = tolerance {
    (tolSec, tolNanosec) = durationComponents(for: tolerance,
                                              clock: clock)
  } else {
    tolSec = 0
    tolNanosec = -1
  }

  let jobID = job.id
  let source = _dispatchEnqueueWithDeadline(CBool(global),
                                            CLongLong(seconds),
                                            CLongLong(nanoseconds),
                                            CLongLong(tolSec),
                                            CLongLong(tolNanosec),
                                            clockID.rawValue,
                                            UnownedJob(job))
  return JobCancellationToken(
    executor: executor, jobID: jobID,
    opaqueData: [source, 0],
    onCancellation: behavior,
    cleanUp: {
      _dispatchReleaseSource($0.opaqueData[opaqueDataDispatchSource])
    }
  )
}

#endif // !$Embedded && !os(WASI) && !os(Emscripten)
