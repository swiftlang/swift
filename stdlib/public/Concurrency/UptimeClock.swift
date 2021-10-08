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

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
public struct UptimeClock {
  public struct Instant: Codable, Sendable {
    var nanoseconds: UInt64
    
    init(nanoseconds: UInt64) {
      self.nanoseconds = nanoseconds
    }
  
    public init?(converting deadline: Date) {
      var absoluteNanoseconds = UInt64(0)
      var realtimeSeconds = Int64(0)
      var realtimeNanoseconds = Int64(0)
      _getTime(
        absoluteNanoseconds: &absoluteNanoseconds,
        continuousNanoseconds: nil,
        realtimeSeconds: &realtimeSeconds, realtimeNanoseconds: &realtimeNanoseconds
      )
      let monotonicNow = Instant(nanoseconds: absoluteNanoseconds)
      let realtimeNow = Date(durationSince1970: .seconds(realtimeSeconds) + .nanoseconds(realtimeNanoseconds))
      
      let duration = realtimeNow.duration(to: deadline)
      if duration > Duration.nanoseconds(Int64.max) {
        return nil
      }
      self = monotonicNow.advanced(by: duration)
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension ClockProtocol where Self == UptimeClock {
  public static var uptime: UptimeClock { return UptimeClock() }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension UptimeClock: ClockProtocol {
  public var now: UptimeClock.Instant {
    UptimeClock.now
  }
  
  public static var now: UptimeClock.Instant {
    var nanoseconds = UInt64(0)
    _getTime(
      absoluteNanoseconds: &nanoseconds,
      continuousNanoseconds: nil,
      realtimeSeconds: nil, realtimeNanoseconds: nil
    )
    return UptimeClock.Instant(nanoseconds: nanoseconds)
  }
  
  public func sleep(until deadline: Instant) async throws {
    try await Task.sleep(until: 0, deadline.nanoseconds, clockId: .uptime)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension UptimeClock.Instant: InstantProtocol {
  public static var now: UptimeClock.Instant { UptimeClock().now }
  
  public func advanced(by duration: Duration) -> UptimeClock.Instant {
    if duration > .zero {
      return UptimeClock.Instant(nanoseconds: nanoseconds + duration.seconds.magnitude * 1000000000 + duration.nanoseconds.magnitude)
    } else {
      return UptimeClock.Instant(nanoseconds: nanoseconds - (duration.seconds.magnitude * 1000000000 + duration.nanoseconds.magnitude))
    }
  }
  
  public func duration(to other: UptimeClock.Instant) -> Duration {
    if other.nanoseconds > nanoseconds {
      return .nanoseconds(other.nanoseconds - nanoseconds)
    } else {
      return .nanoseconds(-Int64(nanoseconds - other.nanoseconds))
    }
  }
  
  public func hash(into hasher: inout Hasher) {
    hasher.combine(nanoseconds)
  }
  
  public static func == (_ lhs: UptimeClock.Instant, _ rhs: UptimeClock.Instant) -> Bool {
    return lhs.nanoseconds == rhs.nanoseconds
  }
  
  public static func < (_ lhs: UptimeClock.Instant, _ rhs: UptimeClock.Instant) -> Bool {
    return lhs.nanoseconds < rhs.nanoseconds
  }
}

