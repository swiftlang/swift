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
public struct ContinuousClock {
  public struct Instant: Codable, Sendable {
    internal var _value: Duration
    
    internal init(_value: Duration) {
      self._value = _value
    }
  }
  
  public init() { }
}

@available(SwiftStdlib 9999, *)
extension Clock where Self == ContinuousClock {
  @available(SwiftStdlib 9999, *)
  public static var continuous: ContinuousClock { return ContinuousClock() }
}

@available(SwiftStdlib 9999, *)
extension ContinuousClock: Clock {
  public var now: ContinuousClock.Instant {
    ContinuousClock.now
  }

  public var minimumResolution: Duration { 
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    _getClockRes(
      seconds: &seconds, 
      nanoseconds: &nanoseconds, 
      clock: .continuous)
    return .seconds(seconds) + .nanoseconds(nanoseconds)
  }
  
  public static var now: ContinuousClock.Instant {
    var seconds = Int64(0)
    var nanoseconds = Int64(0)
    _getClockRes(
      seconds: &seconds, 
      nanoseconds: &nanoseconds, 
      clock: .continuous)
    return ContinuousClock.Instant(_value:
      .seconds(seconds) + .nanoseconds(nanoseconds))
  }
  
  public func sleep(
    until deadline: Instant, tolerance: Duration? = nil
  ) async throws {
    try await Task._sleep(until: 
      deadline._value.seconds, deadline._value.nanoseconds, 
      tolerance: tolerance, 
      clock: .continuous)
  }
}

@available(SwiftStdlib 9999, *)
extension ContinuousClock.Instant: InstantProtocol {
  public static var now: ContinuousClock.Instant { ContinuousClock.now }
  
  public func advanced(by duration: Duration) -> ContinuousClock.Instant {
    return ContinuousClock.Instant(_value: _value + duration)
  }
  
  public func duration(to other: ContinuousClock.Instant) -> Duration {
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
}
