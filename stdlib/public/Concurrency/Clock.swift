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

/// A mechanism in which to measure time, and delay work until a given point
/// in time.
///
/// Types that conform to the `Clock` protocol define a concept of "now" which
/// is the specific instant in time that property is accessed. Any pair of calls
/// to the `now` property may have a minimum duration between them - this
/// minimum resolution is exposed by the `minimumResolution` property to inform
/// any user of the type the expected granularity of accuracy.
///
/// One of the primary uses for clocks is to schedule task sleeping. This method
/// resumes the calling task after a given deadline has been met or passed with
/// a given tolerance value. The tolerance is expected as a leeway around the
/// deadline. The clock may reschedule tasks within the tolerance to ensure
/// efficient execution of resumptions by reducing potential operating system
/// wake-ups. If no tolerance is specified (i.e. nil is passed in) the sleep
/// function is expected to schedule with a default tolerance strategy.
///
/// For more information about specific clocks see `ContinuousClock` and
/// `SuspendingClock`.
@available(SwiftStdlibCurrentOS 5.7, *)
public protocol Clock<Duration>: Sendable {
  associatedtype Duration
  associatedtype Instant: InstantProtocol where Instant.Duration == Duration

  var now: Instant { get }
  var minimumResolution: Instant.Duration { get }

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
  func sleep(until deadline: Instant, tolerance: Instant.Duration?) async throws
#endif

  /// The traits associated with this clock instance.
  @available(SwiftStdlibCurrentOS 6.2, *)
  var traits: ClockTraits { get }

  /// Convert a Clock-specific Duration to a Swift Duration
  ///
  /// Some clocks may define `C.Duration` to be something other than a
  /// `Swift.Duration`, but that makes it tricky to convert timestamps
  /// between clocks, which is something we want to be able to support.
  /// This method will convert whatever `C.Duration` is to a `Swift.Duration`.
  ///
  /// Parameters:
  ///
  /// - from duration: The `Duration` to convert
  ///
  /// Returns: A `Swift.Duration` representing the equivalent duration, or
  ///          `nil` if this function is not supported.
  @available(SwiftStdlibCurrentOS 6.2, *)
  func convert(from duration: Duration) -> Swift.Duration?

  /// Convert a Swift Duration to a Clock-specific Duration
  ///
  /// Parameters:
  ///
  /// - from duration: The `Swift.Duration` to convert.
  ///
  /// Returns: A `Duration` representing the equivalent duration, or
  ///          `nil` if this function is not supported.
  @available(SwiftStdlibCurrentOS 6.2, *)
  func convert(from duration: Swift.Duration) -> Duration?

  /// Convert an `Instant` from some other clock's `Instant`
  ///
  /// Parameters:
  ///
  /// - instant:    The instant to convert.
  //  - from clock: The clock to convert from.
  ///
  /// Returns: An `Instant` representing the equivalent instant, or
  ///          `nil` if this function is not supported.
  @available(SwiftStdlibCurrentOS 6.2, *)
  func convert<OtherClock: Clock>(instant: OtherClock.Instant,
                                  from clock: OtherClock) -> Instant?
}

@available(SwiftStdlibCurrentOS 5.7, *)
extension Clock {
  /// Measure the elapsed time to execute a closure.
  ///
  ///       let clock = ContinuousClock()
  ///       let elapsed = clock.measure {
  ///          someWork()
  ///       }
  @available(SwiftStdlibCurrentOS 5.7, *)
  public func measure(_ work: () throws -> Void) rethrows -> Instant.Duration {
    let start = now
    try work()
    let end = now
    return start.duration(to: end)
  }

  /// Measure the elapsed time to execute an asynchronous closure.
  ///
  ///       let clock = ContinuousClock()
  ///       let elapsed = await clock.measure {
  ///          await someWork()
  ///       }
  @available(SwiftStdlibCurrentOS 5.7, *)
  @_alwaysEmitIntoClient
  public func measure(
    isolation: isolated (any Actor)? = #isolation,
    _ work: () async throws -> Void
  ) async rethrows -> Instant.Duration {
    let start = now
    try await work()
    let end = now
    return start.duration(to: end)
  }

  // Note: hack to stage out @_unsafeInheritExecutor forms of various functions
  // in favor of #isolation. The _unsafeInheritExecutor_ prefix is meaningful
  // to the type checker.
  //
  // This function also doubles as an ABI-compatibility shim predating the
  // introduction of #isolation.
  @available(SwiftStdlibCurrentOS 5.7, *)
  @_silgen_name("$ss5ClockPsE7measurey8DurationQzyyYaKXEYaKF")
  @_unsafeInheritExecutor // for ABI compatibility
  public func _unsafeInheritExecutor_measure(
    _ work: () async throws -> Void
  ) async rethrows -> Instant.Duration {
    let start = now
    try await work()
    let end = now
    return start.duration(to: end)
  }
}

@available(SwiftStdlibCurrentOS 6.2, *)
extension Clock {
  // For compatibility, return `nil` if this is not implemented
  public func convert(from duration: Duration) -> Swift.Duration? {
    return nil
  }

  public func convert(from duration: Swift.Duration) -> Duration? {
    return nil
  }

  public func convert<OtherClock: Clock>(instant: OtherClock.Instant,
                                  from clock: OtherClock) -> Instant? {
    let ourNow = now
    let otherNow = clock.now
    let otherDuration = otherNow.duration(to: instant)

    // Convert to `Swift.Duration`
    guard let duration = clock.convert(from: otherDuration) else {
      return nil
    }

    // Convert from `Swift.Duration`
    guard let ourDuration = convert(from: duration) else {
      return nil
    }

    return ourNow.advanced(by: ourDuration)
  }
}

@available(SwiftStdlibCurrentOS 6.2, *)
extension Clock where Duration == Swift.Duration {
  public func convert(from duration: Duration) -> Duration? {
    return duration
  }
}

#if !SWIFT_STDLIB_TASK_TO_THREAD_MODEL_CONCURRENCY
@available(SwiftStdlibCurrentOS 5.7, *)
extension Clock {
  /// Suspends for the given duration.
  ///
  /// Prefer to use the `sleep(until:tolerance:)` method on `Clock` if you have
  /// access to an absolute instant.
  @available(SwiftStdlibCurrentOS 5.7, *)
  @_alwaysEmitIntoClient
  public func sleep(
    for duration: Instant.Duration,
    tolerance: Instant.Duration? = nil
  ) async throws {
    try await sleep(until: now.advanced(by: duration), tolerance: tolerance)
  }
}
#endif

/// Represents traits of a particular Clock implementation.
///
/// Clocks may be of a number of different varieties; executors will likely
/// have specific clocks that they can use to schedule jobs, and will
/// therefore need to be able to convert timestamps to an appropriate clock
/// when asked to enqueue a job with a delay or deadline.
///
/// Choosing a clock in general requires the ability to tell which of their
/// clocks best matches the clock that the user is trying to specify a
/// time or delay in.  Executors are expected to do this on a best effort
/// basis.
@available(SwiftStdlibCurrentOS 6.2, *)
public struct ClockTraits: OptionSet {
  public let rawValue: UInt32

  public init(rawValue: UInt32) {
    self.rawValue = rawValue
  }

  /// Clocks with this trait continue running while the machine is asleep.
  public static let continuous = ClockTraits(rawValue: 1 << 0)

  /// Indicates that a clock's time will only ever increase.
  public static let monotonic = ClockTraits(rawValue: 1 << 1)

  /// Clocks with this trait are tied to "wall time".
  public static let wallTime = ClockTraits(rawValue: 1 << 2)
}

@available(SwiftStdlibCurrentOS 6.2, *)
extension Clock {
  /// The traits associated with this clock instance.
  @available(SwiftStdlibCurrentOS 6.2, *)
  public var traits: ClockTraits {
    return []
  }
}

enum _ClockID: Int32 {
  case continuous = 1
  case suspending = 2
}

@available(SwiftStdlibCurrentOS 5.7, *)
@_silgen_name("swift_get_time")
internal func _getTime(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: CInt)

@available(SwiftStdlibCurrentOS 5.7, *)
@_silgen_name("swift_get_clock_res")
internal func _getClockRes(
  seconds: UnsafeMutablePointer<Int64>,
  nanoseconds: UnsafeMutablePointer<Int64>,
  clock: CInt)

@available(SwiftStdlibCurrentOS 6.2, *)
@_silgen_name("swift_sleep")
internal func _sleep(
  seconds: Int64,
  nanoseconds: Int64)
