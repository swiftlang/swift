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

@_silgen_name("swift_get_time")
@usableFromInline
func _getTime(
  absoluteNanoseconds: UnsafeMutablePointer<UInt64>?,
  continuousNanoseconds: UnsafeMutablePointer<UInt64>?,
  realtimeSeconds: UnsafeMutablePointer<Int64>?,
  realtimeNanoseconds: UnsafeMutablePointer<Int64>?
)

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
public struct MonotonicClock {
  public struct Instant: Codable, Sendable {
    var nanoseconds: UInt64
    
    init(nanoseconds: UInt64) {
      self.nanoseconds = nanoseconds
    }
    
    public init?(converting deadline: _Date) {
      var monotonicNanoseconds = UInt64(0)
      var realtimeSeconds = Int64(0)
      var realtimeNanoseconds = Int64(0)
      _getTime(
        absoluteNanoseconds: nil,
        continuousNanoseconds: &monotonicNanoseconds,
        realtimeSeconds: &realtimeSeconds, realtimeNanoseconds: &realtimeNanoseconds
      )
      let monotonicNow = Instant(nanoseconds: monotonicNanoseconds)
      let realtimeNow = _Date(durationSince1970: .seconds(realtimeSeconds) + .nanoseconds(realtimeNanoseconds))
      
      let duration = realtimeNow.duration(to: deadline)
      if duration > Duration.nanoseconds(Int64.max) {
        return nil
      }
      self = monotonicNow.advanced(by: duration)
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension _Date {
  public init(converting deadline: MonotonicClock.Instant) {
    var monotonicNanoseconds = UInt64(0)
    var realtimeSeconds = Int64(0)
    var realtimeNanoseconds = Int64(0)
    _getTime(
      absoluteNanoseconds: nil,
      continuousNanoseconds: &monotonicNanoseconds,
      realtimeSeconds: &realtimeSeconds, realtimeNanoseconds: &realtimeNanoseconds
    )
    
    let realtimeNow = _Date(durationSince1970: .seconds(realtimeSeconds) + .nanoseconds(realtimeNanoseconds))
    let monotonicNow = MonotonicClock.Instant(nanoseconds: monotonicNanoseconds)
    let duration = monotonicNow.duration(to: deadline)
    self = realtimeNow.advanced(by: .nanoseconds(duration.nanoseconds))
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension ClockProtocol where Self == MonotonicClock {
  public static var monotonic: MonotonicClock { return MonotonicClock() }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension MonotonicClock: ClockProtocol {
  public var now: MonotonicClock.Instant {
    MonotonicClock.now
  }
  
  public static var now: MonotonicClock.Instant {
    var nanoseconds = UInt64(0)
    _getTime(
      absoluteNanoseconds: nil,
      continuousNanoseconds: &nanoseconds,
      realtimeSeconds: nil, realtimeNanoseconds: nil
    )
    return MonotonicClock.Instant(nanoseconds: nanoseconds)
  }
  
  public func sleep(until deadline: Instant) async throws {
    try await Task.sleep(until: 0, deadline.nanoseconds, clockId: .monotonic)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension MonotonicClock.Instant: InstantProtocol {
  public static var now: MonotonicClock.Instant { MonotonicClock.now }
  
  public func advanced(by duration: Duration) -> MonotonicClock.Instant {
    if duration > .zero {
      return MonotonicClock.Instant(nanoseconds: nanoseconds + duration.seconds.magnitude * 1000000000 + duration.nanoseconds.magnitude)
    } else {
      return MonotonicClock.Instant(nanoseconds: nanoseconds - (duration.seconds.magnitude * 1000000000 + duration.nanoseconds.magnitude))
    }
  }
  
  public func duration(to other: MonotonicClock.Instant) -> Duration {
    if other.nanoseconds > nanoseconds {
      return .nanoseconds(other.nanoseconds - nanoseconds)
    } else {
      return .nanoseconds(-Int64(nanoseconds - other.nanoseconds))
    }
  }
  
  public func hash(into hasher: inout Hasher) {
    hasher.combine(nanoseconds)
  }
  
  public static func == (_ lhs: MonotonicClock.Instant, _ rhs: MonotonicClock.Instant) -> Bool {
    return lhs.nanoseconds == rhs.nanoseconds
  }
  
  public static func < (_ lhs: MonotonicClock.Instant, _ rhs: MonotonicClock.Instant) -> Bool {
    return lhs.nanoseconds < rhs.nanoseconds
  }
}
