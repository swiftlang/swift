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

@available(SwiftStdlib 9999, *)
public struct SuspendingClock {
  public struct Instant: Codable, Sendable {
    internal var _value: Swift.Duration

    internal init(_value: Swift.Duration) {
      self._value = _value
    }
  }

  public init() { }
}

@available(SwiftStdlib 9999, *)
extension Clock where Self == SuspendingClock {
  @available(SwiftStdlib 9999, *)
  public static var suspending: SuspendingClock { return SuspendingClock() }
}

@available(SwiftStdlib 9999, *)
extension SuspendingClock: Clock {
  @available(SwiftStdlib 9999, *)
  public var now: SuspendingClock.Instant {
    SuspendingClock.now
  }

  @available(SwiftStdlib 9999, *)
  public static var now: SuspendingClock.Instant {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    _getTime(
      seconds: &seconds,
      nanoseconds: &nanoseconds,
      clock: .suspending)
    return SuspendingClock.Instant(_value:
      .seconds(seconds) + .nanoseconds(nanoseconds))
  }

  @available(SwiftStdlib 9999, *)
  public var minimumResolution: Swift.Duration {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    _getClockRes(
      seconds: &seconds,
      nanoseconds: &nanoseconds,
      clock: .suspending)
    return .seconds(seconds) + .nanoseconds(nanoseconds)
  }

  @available(SwiftStdlib 9999, *)
  public func sleep(
    until deadline: Instant, tolerance: Swift.Duration? = nil
  ) async throws {
    let (seconds, attoseconds) = deadline._value.components
    let nanoseconds = attoseconds / 1_000_000_000
    try await Task._sleep(until:seconds, nanoseconds,
      tolerance: tolerance,
      clock: .suspending)
  }
}

@available(SwiftStdlib 9999, *)
extension SuspendingClock.Instant: InstantProtocol {
  @available(SwiftStdlib 9999, *)
  public static var now: SuspendingClock.Instant { SuspendingClock().now }

  @available(SwiftStdlib 9999, *)
  public func advanced(by duration: Swift.Duration) -> SuspendingClock.Instant {
    SuspendingClock.Instant(_value: _value + duration)
  }

  @available(SwiftStdlib 9999, *)
  public func duration(to other: SuspendingClock.Instant) -> Swift.Duration {
    other._value - _value
  }

  @available(SwiftStdlib 9999, *)
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_value)
  }

  @available(SwiftStdlib 9999, *)
  public static func == (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Bool {
    return lhs._value == rhs._value
  }

  @available(SwiftStdlib 9999, *)
  public static func < (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Bool {
    return lhs._value < rhs._value
  }

  @available(SwiftStdlib 9999, *)
  public static func + (
    _ lhs: SuspendingClock.Instant, _ rhs: Swift.Duration
  ) -> SuspendingClock.Instant {
    lhs.advanced(by: rhs)
  }

  @available(SwiftStdlib 9999, *)
  public static func += (
    _ lhs: inout SuspendingClock.Instant, _ rhs: Swift.Duration
  ) {
    lhs = lhs.advanced(by: rhs)
  }

  @available(SwiftStdlib 9999, *)
  public static func - (
    _ lhs: SuspendingClock.Instant, _ rhs: Swift.Duration
  ) -> SuspendingClock.Instant {
    lhs.advanced(by: .zero - rhs)
  }

  @available(SwiftStdlib 9999, *)
  public static func -= (
    _ lhs: inout SuspendingClock.Instant, _ rhs: Swift.Duration
  ) {
    lhs = lhs.advanced(by: .zero - rhs)
  }

  @available(SwiftStdlib 9999, *)
  public static func - (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Swift.Duration {
    rhs.duration(to: lhs)
  }
}

