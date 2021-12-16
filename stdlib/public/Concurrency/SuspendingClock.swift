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

@available(SwiftStdlib 5.1, *)
public struct SuspendingClock {
  public struct Instant: Codable, Sendable {
    var value: Duration
    
    init(_ value: Duration) {
      self.value = value
    }
  }
  
  public init() { }
}

@available(SwiftStdlib 5.1, *)
extension Clock where Self == SuspendingClock {
  public static var suspending: SuspendingClock { return SuspendingClock() }
}

@available(SwiftStdlib 5.1, *)
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
      clock: .continuous)
    return SuspendingClock.Instant(
      .seconds(seconds) + .nanoseconds(nanoseconds))
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

  public func sleep(
    until deadline: Instant, tolerance: Duration? = nil
  ) async throws {
    try await Task.sleep(until: 
      deadline.value.seconds, deadline.value.nanoseconds, 
      tolerance: tolerance, 
      clock: .suspending)
  }
}

@available(SwiftStdlib 5.1, *)
extension SuspendingClock.Instant: InstantProtocol {
  public static var now: SuspendingClock.Instant { SuspendingClock().now }
  
  public func advanced(by duration: Duration) -> SuspendingClock.Instant {
    SuspendingClock.Instant(value + duration)
  }
  
  public func duration(to other: SuspendingClock.Instant) -> Duration {
    other.value - value
  }
  
  public func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
  
  public static func == (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Bool {
    return lhs.value == rhs.value
  }
  
  public static func < (
    _ lhs: SuspendingClock.Instant, _ rhs: SuspendingClock.Instant
  ) -> Bool {
    return lhs.value < rhs.value
  }
}

