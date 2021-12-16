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
public struct ContinuousClock {
  public struct Instant: Codable, Sendable {
    var value: Duration
    
    init(_ value: Duration) {
      self.value = value
    }
  }
  
  public init() { }
}

@available(SwiftStdlib 5.1, *)
extension Clock where Self == ContinuousClock {
  @available(SwiftStdlib 5.1, *)
  public static var continuous: ContinuousClock { return ContinuousClock() }
}

@available(SwiftStdlib 5.1, *)
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
    return ContinuousClock.Instant(
      .seconds(seconds) + .nanoseconds(nanoseconds))
  }
  
  public func sleep(
    until deadline: Instant, tolerance: Duration? = nil
  ) async throws {
    try await Task.sleep(until: 
      deadline.value.seconds, deadline.value.nanoseconds, 
      tolerance: tolerance, 
      clock: .continuous)
  }
}

@available(SwiftStdlib 5.1, *)
extension ContinuousClock.Instant: InstantProtocol {
  public static var now: ContinuousClock.Instant { ContinuousClock.now }
  
  public func advanced(by duration: Duration) -> ContinuousClock.Instant {
    return ContinuousClock.Instant(value + duration)
  }
  
  public func duration(to other: ContinuousClock.Instant) -> Duration {
    other.value - value
  }
  
  public func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
  
  public static func == (
    _ lhs: ContinuousClock.Instant, _ rhs: ContinuousClock.Instant
  ) -> Bool {
    return lhs.value == rhs.value
  }
  
  public static func < (
    _ lhs: ContinuousClock.Instant, _ rhs: ContinuousClock.Instant
  ) -> Bool {
    return lhs.value < rhs.value
  }
}
