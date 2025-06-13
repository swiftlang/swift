//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
import Swift

/// A clock that measures time that always increments and does not stop
/// incrementing while the system is asleep.
///
/// `ContinuousClock` can be considered as a stopwatch style time. The frame of
/// reference of the `Instant` may be bound to process launch, machine boot or
/// some other locally defined reference point. This means that the instants are
/// only comparable locally during the execution of a program.
///
/// This clock is suitable for high resolution measurements of execution.
@available(StdlibDeploymentTarget 5.7, *)
@_unavailableInEmbedded
public struct ContinuousClock: Sendable {
  /// A continuous point in time used for `ContinuousClock`.
  public struct Instant: Sendable {
    internal var _value: Swift.Duration

    internal init(_value: Swift.Duration) {
      self._value = _value
    }
  }

  public init() { }
}

#if !$Embedded
@available(StdlibDeploymentTarget 5.7, *)
extension ContinuousClock.Instant: Codable {
}
#endif

@available(StdlibDeploymentTarget 5.7, *)
extension Duration {
  internal init(_seconds s: Int64, nanoseconds n: Int64) {
    let (secHi, secLo) = s.multipliedFullWidth(by: 1_000_000_000_000_000_000)
    // _nanoseconds is in 0 ..< 1_000_000_000, so the conversion to UInt64
    // and multiply cannot overflow. If you somehow trap here, it is because
    // the underlying clock hook that produced the time value is implemented
    // incorrectly on your platform, but because we trap we can't silently
    // get bogus data.
    let (low, carry) = secLo.addingReportingOverflow(UInt64(n) * 1_000_000_000)
    let high = secHi &+ (carry ? 1 : 0)
    self.init(_high: high, low: low)
  }
}

@available(StdlibDeploymentTarget 5.7, *)
@_unavailableInEmbedded
extension Clock where Self == ContinuousClock {
  /// A clock that measures time that always increments but does not stop
  /// incrementing while the system is asleep.
  ///
  ///       try await Task.sleep(until: .now + .seconds(3), clock: .continuous)
  ///
  @available(StdlibDeploymentTarget 5.7, *)
  public static var continuous: ContinuousClock { return ContinuousClock() }
}

@available(StdlibDeploymentTarget 5.7, *)
@_unavailableInEmbedded
extension ContinuousClock: Clock {
  /// The current continuous instant.
  public var now: ContinuousClock.Instant {
    ContinuousClock.now
  }

  /// The minimum non-zero resolution between any two calls to `now`.
  public var minimumResolution: Swift.Duration {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    unsafe _getClockRes(
      seconds: &seconds,
      nanoseconds: &nanoseconds,
      clock: _ClockID.continuous.rawValue)
    return Duration(_seconds: seconds, nanoseconds: nanoseconds)
  }

  /// The current continuous instant.
  public static var now: ContinuousClock.Instant {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    unsafe _getTime(
      seconds: &seconds,
      nanoseconds: &nanoseconds,
      clock: _ClockID.continuous.rawValue)
    return Instant(
      _value: Duration(_seconds: seconds, nanoseconds: nanoseconds)
    )
  }

  /// The continuous clock is continuous and monotonic
  @_spi(CustomDefaultExecutors)
  @available(StdlibDeploymentTarget 6.2, *)
  public var traits: ClockTraits {
    return [.continuous, .monotonic]
  }

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  /// Suspend task execution until a given deadline within a tolerance.
  /// If no tolerance is specified then the system may adjust the deadline
  /// to coalesce CPU wake-ups to more efficiently process the wake-ups in
  /// a more power efficient manner.
  ///
  /// If the task is canceled before the time ends, this function throws
  /// `CancellationError`.
  ///
  /// This function doesn't block the underlying thread.
  public func sleep(
    until deadline: Instant, tolerance: Swift.Duration? = nil
  ) async throws {
    if #available(StdlibDeploymentTarget 6.2, *) {
      try await Task._sleep(until: deadline,
                            tolerance: tolerance,
                            clock: self)
    } else {
      fatalError("we should never get here; if we have, availability is broken")
    }
  }
#else
  @available(StdlibDeploymentTarget 5.7, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func sleep(
    until deadline: Instant, tolerance: Swift.Duration? = nil
  ) async throws {
      fatalError("Unavailable in task-to-thread concurrency model")
  }
#endif
}

@available(StdlibDeploymentTarget 5.7, *)
@_unavailableInEmbedded
extension ContinuousClock {
  @available(SwiftStdlib 5.7, *)
  @_alwaysEmitIntoClient
  public var systemEpoch: Instant {
    unsafe unsafeBitCast(Duration.seconds(0), to: Instant.self)
  }
}

@available(SwiftStdlib 5.7, *)
@_unavailableInEmbedded
extension ContinuousClock.Instant: InstantProtocol {
  public static var now: ContinuousClock.Instant { ContinuousClock.now }

  public func advanced(by duration: Swift.Duration) -> ContinuousClock.Instant {
    return ContinuousClock.Instant(_value: _value + duration)
  }

  public func duration(to other: ContinuousClock.Instant) -> Swift.Duration {
    other._value - _value
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(_value)
  }

  public static func == (
    _ lhs: ContinuousClock.Instant, _ rhs: ContinuousClock.Instant
  ) -> Bool {
    return lhs._value == rhs._value
  }

  public static func < (
    _ lhs: ContinuousClock.Instant, _ rhs: ContinuousClock.Instant
  ) -> Bool {
    return lhs._value < rhs._value
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func + (
    _ lhs: ContinuousClock.Instant, _ rhs: Swift.Duration
  ) -> ContinuousClock.Instant {
    lhs.advanced(by: rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func += (
    _ lhs: inout ContinuousClock.Instant, _ rhs: Swift.Duration
  ) {
    lhs = lhs.advanced(by: rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func - (
    _ lhs: ContinuousClock.Instant, _ rhs: Swift.Duration
  ) -> ContinuousClock.Instant {
    lhs.advanced(by: .zero - rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func -= (
    _ lhs: inout ContinuousClock.Instant, _ rhs: Swift.Duration
  ) {
    lhs = lhs.advanced(by: .zero - rhs)
  }

  @_alwaysEmitIntoClient
  @inlinable
  public static func - (
    _ lhs: ContinuousClock.Instant, _ rhs: ContinuousClock.Instant
  ) -> Swift.Duration {
    rhs.duration(to: lhs)
  }
}
