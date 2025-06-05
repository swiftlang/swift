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

@available(StdlibDeploymentTarget 6.2, *)
public class DispatchMainExecutor: RunLoopExecutor, @unchecked Sendable {
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
extension DispatchMainExecutor: SchedulableExecutor {
  public var asSchedulable: SchedulableExecutor? {
    return self
  }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    let tolSec, tolNanosec: CLongLong
    if let tolerance = tolerance {
      (tolSec, tolNanosec) = delay(from: tolerance, clock: clock)
    } else {
      tolSec = 0
      tolNanosec = -1
    }

    let (clockID, seconds, nanoseconds) = timestamp(for: instant, clock: clock)

    _dispatchEnqueueWithDeadline(CBool(false),
                                 CLongLong(seconds), CLongLong(nanoseconds),
                                 CLongLong(tolSec), CLongLong(tolNanosec),
                                 clockID.rawValue,
                                 UnownedJob(job))
  }
}

@available(StdlibDeploymentTarget 6.2, *)
extension DispatchMainExecutor: MainExecutor {}

// .. Task Executor ............................................................

@available(StdlibDeploymentTarget 6.2, *)
public class DispatchGlobalTaskExecutor: TaskExecutor, SchedulableExecutor,
                                         @unchecked Sendable {
  public init() {}

  public func enqueue(_ job: consuming ExecutorJob) {
    _dispatchEnqueueGlobal(UnownedJob(job))
  }

  public var isMainExecutor: Bool { false }

  public func enqueue<C: Clock>(_ job: consuming ExecutorJob,
                                at instant: C.Instant,
                                tolerance: C.Duration? = nil,
                                clock: C) {
    let tolSec, tolNanosec: CLongLong
    if let tolerance = tolerance {
      (tolSec, tolNanosec) = delay(from: tolerance, clock: clock)
    } else {
      tolSec = 0
      tolNanosec = -1
    }

    let (clockID, seconds, nanoseconds) = timestamp(for: instant, clock: clock)

    _dispatchEnqueueWithDeadline(CBool(true),
                                 CLongLong(seconds), CLongLong(nanoseconds),
                                 CLongLong(tolSec), CLongLong(tolNanosec),
                                 clockID.rawValue,
                                 UnownedJob(job))
  }
}

// .. Clock Support ............................................................

/// DispatchMainExecutor and DispatchTaskExecutor both implement this
/// protocol.
///
/// It is used to help convert instants and durations from arbitrary `Clock`s
/// to Dispatch's time base.
@available(StdlibDeploymentTarget 6.2, *)
protocol DispatchExecutorProtocol: Executor {

  /// Convert an `Instant` from the specified clock to a tuple identifying
  /// the Dispatch clock and the seconds and nanoseconds components.
  ///
  /// Parameters:
  ///
  /// - for instant: The `Instant` to convert.
  /// - clock:       The `Clock` instant that the `Instant` came from.
  ///
  /// Returns: A tuple of `(clockID, seconds, nanoseconds)`.
  func timestamp<C: Clock>(for instant: C.Instant, clock: C)
    -> (clockID: DispatchClockID, seconds: Int64, nanoseconds: Int64)

  /// Convert a `Duration` from the specified clock to a tuple containing
  /// seconds and nanosecond components.
  func delay<C: Clock>(from duration: C.Duration, clock: C)
    -> (seconds: Int64, nanoseconds: Int64)

}

/// An enumeration identifying one of the Dispatch-supported clocks
enum DispatchClockID: CInt {
  case continuous = 1
  case suspending = 2
}

@available(StdlibDeploymentTarget 6.2, *)
extension DispatchExecutorProtocol {

  func clamp(_ components: (seconds: Int64, attoseconds: Int64))
    -> (seconds: Int64, attoseconds: Int64) {
    if components.seconds < 0
         || components.seconds == 0 && components.attoseconds < 0 {
      return (seconds: 0, attoseconds: 0)
    }
    return (seconds: components.seconds, attoseconds: components.attoseconds)
  }

  func timestamp<C: Clock>(for instant: C.Instant, clock: C)
    -> (clockID: DispatchClockID, seconds: Int64, nanoseconds: Int64) {
    if clock.traits.contains(.continuous) {
        let dispatchClock: ContinuousClock = .continuous
        let instant = dispatchClock.convert(instant: instant, from: clock)!
        let (seconds, attoseconds) = clamp(instant._value.components)
        let nanoseconds = attoseconds / 1_000_000_000
        return (clockID: .continuous,
                seconds: Int64(seconds),
                nanoseconds: Int64(nanoseconds))
    } else {
        let dispatchClock: SuspendingClock = .suspending
        let instant = dispatchClock.convert(instant: instant, from: clock)!
        let (seconds, attoseconds) = clamp(instant._value.components)
        let nanoseconds = attoseconds / 1_000_000_000
        return (clockID: .suspending,
                seconds: Int64(seconds),
                nanoseconds: Int64(nanoseconds))
    }
  }

  func delay<C: Clock>(from duration: C.Duration, clock: C)
    -> (seconds: Int64, nanoseconds: Int64) {
    let swiftDuration = clock.convert(from: duration)!
    let (seconds, attoseconds) = clamp(swiftDuration.components)
    let nanoseconds = attoseconds / 1_000_000_000
    return (seconds: seconds, nanoseconds: nanoseconds)
  }

}

@available(StdlibDeploymentTarget 6.2, *)
extension DispatchGlobalTaskExecutor: DispatchExecutorProtocol {
}

@available(StdlibDeploymentTarget 6.2, *)
extension DispatchMainExecutor: DispatchExecutorProtocol {
}

#endif // !$Embedded && !os(WASI)
