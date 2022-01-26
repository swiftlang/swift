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
    internal var _value: Duration
    
    internal init(_value: Duration) {
      self._value = _value
    }
  }
  
  public init() { }
}

@available(SwiftStdlib 9999, *)
extension Clock where Self == SuspendingClock {
  public static var suspending: SuspendingClock { return SuspendingClock() }
}

@available(SwiftStdlib 9999, *)
extension SuspendingClock: Clock {
  public var now: SuspendingClock.Instant {
    SuspendingClock.now
  }
  
  public static var now: SuspendingClock.Instant {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    _getClockRes(
      seconds: &seconds, 
      nanoseconds: &nanoseconds, 
      clock: .suspending)
    return SuspendingClock.Instant(_value:
      .seconds(seconds) + .nanoseconds(nanoseconds))
  }

  public var minimumResolution: Duration { 
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    _getClockRes(
      seconds: &seconds, 
      nanoseconds: &nanoseconds, 
      clock: .suspending)
    return .seconds(seconds) + .nanoseconds(nanoseconds)
  }

  public func sleep(
    until deadline: Instant, tolerance: Duration? = nil
  ) async throws {
    try await Task._sleep(until: 
      deadline._value.seconds, deadline._value.nanoseconds, 
      tolerance: tolerance, 
      clock: .suspending)
  }
}

@available(SwiftStdlib 9999, *)
extension SuspendingClock.Instant: InstantProtocol {
  public static var now: SuspendingClock.Instant { SuspendingClock().now }
  
  public func advanced(by duration: Duration) -> SuspendingClock.Instant {
    SuspendingClock.Instant(_value: _value + duration)
  }
  
  public func duration(to other: SuspendingClock.Instant) -> Duration {
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
  
  @_alwaysEmitIntoClient
  @inlinable  
  public static func + (
    _ lhs: SuspendingClock.Instant, _ rhs: Duration
  ) -> SuspendingClock.Instant {
    lhs.advanced(by: rhs)
  }
  
  @_alwaysEmitIntoClient
  @inlinable
  public static func += (
    _ lhs: inout SuspendingClock.Instant, _ rhs: Duration
  ) {
    lhs = lhs.advanced(by: rhs)
  }
  
  @_alwaysEmitIntoClient
  @inlinable
  public static func - (
    _ lhs: SuspendingClock.Instant, _ rhs: Duration
  ) -> SuspendingClock.Instant {
    lhs.advanced(by: .zero - rhs)
  }
  
  @_alwaysEmitIntoClient
  @inlinable
  public static func -= (
    _ lhs: inout SuspendingClock.Instant, _ rhs: Duration
  ) {
    lhs = lhs.advanced(by: .zero - rhs)
  }
  
  @_alwaysEmitIntoClient
  @inlinable
  public static func - (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Duration {
    rhs.duration(to: lhs)
  }
}

