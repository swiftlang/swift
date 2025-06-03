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

/// A clock that measures time that always increments but stops incrementing
/// while the system is asleep.
///
/// `SuspendingClock` can be considered as a system awake time clock. The frame
/// of reference of the `Instant` may be bound machine boot or some other
/// locally defined reference point. This means that the instants are
/// only comparable on the same machine in the same booted session.
///
/// This clock is suitable for high resolution measurements of execution.
@available(SwiftStdlibCurrentOS 5.7, *)
@_unavailableInEmbedded
public struct SuspendingClock: Sendable {
  public struct Instant: Sendable {
    internal var _value: Swift.Duration

    internal init(_value: Swift.Duration) {
      self._value = _value
    }
  }

  public init() { }
}

#if !$Embedded
@available(SwiftStdlibCurrentOS 5.7, *)
extension SuspendingClock.Instant: Codable {
}
#endif

@available(SwiftStdlibCurrentOS 5.7, *)
@_unavailableInEmbedded
extension Clock where Self == SuspendingClock {
  /// A clock that measures time that always increments but stops incrementing
  /// while the system is asleep.
  ///
  ///       try await Task.sleep(until: .now + .seconds(3), clock: .suspending)
  ///
  @available(SwiftStdlibCurrentOS 5.7, *)
  public static var suspending: SuspendingClock { return SuspendingClock() }
}

@available(SwiftStdlibCurrentOS 5.7, *)
@_unavailableInEmbedded
extension SuspendingClock: Clock {
  /// The current instant accounting for machine suspension.
  @available(SwiftStdlibCurrentOS 5.7, *)
  public var now: SuspendingClock.Instant {
    SuspendingClock.now
  }

  /// The current instant accounting for machine suspension.
  @available(SwiftStdlibCurrentOS 5.7, *)
  public static var now: SuspendingClock.Instant {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    unsafe _getTime(
      seconds: &seconds,
      nanoseconds: &nanoseconds,
      clock: _ClockID.suspending.rawValue)
    return Instant(
      _value: Duration(_seconds: seconds, nanoseconds: nanoseconds)
    )
  }

  /// The minimum non-zero resolution between any two calls to `now`.
  @available(SwiftStdlibCurrentOS 5.7, *)
  public var minimumResolution: Swift.Duration {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    unsafe _getClockRes(
      seconds: &seconds,
      nanoseconds: &nanoseconds,
      clock: _ClockID.suspending.rawValue)
    return Duration(_seconds: seconds, nanoseconds: nanoseconds)
  }

  /// The suspending clock is monotonic
  @available(SwiftStdlibCurrentOS 6.2, *)
  public var traits: ClockTraits {
    return [.monotonic]
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
  @available(SwiftStdlibCurrentOS 5.7, *)
  public func sleep(
    until deadline: Instant, tolerance: Swift.Duration? = nil
  ) async throws {
    if #available(SwiftStdlibCurrentOS 6.2, *) {
      try await Task._sleep(until: deadline,
                            tolerance: tolerance,
                            clock: self)
    } else {
      fatalError("we shouldn't get here; if we have, availability is broken")
    }
  }
#else
  @available(SwiftStdlibCurrentOS 5.7, *)
  @available(*, unavailable, message: "Unavailable in task-to-thread concurrency model")
  public func sleep(
    until deadline: Instant, tolerance: Swift.Duration? = nil
  ) async throws {
      fatalError("Unavailable in task-to-thread concurrency model")
  }
#endif
}

@available(SwiftStdlibCurrentOS 5.7, *)
@_unavailableInEmbedded
extension SuspendingClock.Instant: InstantProtocol {
  public static var now: SuspendingClock.Instant { SuspendingClock().now }

  public func advanced(by duration: Swift.Duration) -> SuspendingClock.Instant {
    SuspendingClock.Instant(_value: _value + duration)
  }

  public func duration(to other: SuspendingClock.Instant) -> Swift.Duration {
    other._value - _value
  }

  public func hash(into hasher: inout Hasher) {
    hasher.combine(_value)
  }

  public static func == (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Bool {
    return lhs._value == rhs._value
  }

  public static func < (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Bool {
    return lhs._value < rhs._value
  }

  public static func + (
    _ lhs: SuspendingClock.Instant, _ rhs: Swift.Duration
  ) -> SuspendingClock.Instant {
    lhs.advanced(by: rhs)
  }

  public static func += (
    _ lhs: inout SuspendingClock.Instant, _ rhs: Swift.Duration
  ) {
    lhs = lhs.advanced(by: rhs)
  }

  public static func - (
    _ lhs: SuspendingClock.Instant, _ rhs: Swift.Duration
  ) -> SuspendingClock.Instant {
    lhs.advanced(by: .zero - rhs)
  }

  public static func -= (
    _ lhs: inout SuspendingClock.Instant, _ rhs: Swift.Duration
  ) {
    lhs = lhs.advanced(by: .zero - rhs)
  }

  public static func - (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Swift.Duration {
    rhs.duration(to: lhs)
  }
}
