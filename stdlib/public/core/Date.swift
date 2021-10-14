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

@_silgen_name("swift_get_time")
@usableFromInline
func _getTime(
  absoluteNanoseconds: UnsafeMutablePointer<UInt64>?,
  continuousNanoseconds: UnsafeMutablePointer<UInt64>?,
  realtimeSeconds: UnsafeMutablePointer<Int64>?,
  realtimeNanoseconds: UnsafeMutablePointer<Int64>?
)

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
public struct _Date {
  public var durationSince1970: Duration
  
  public init(durationSince1970: Duration) {
    self.durationSince1970 = durationSince1970
  }

  public static var now: _Date { 
    var realtimeSeconds = Int64(0)
    var realtimeNanoseconds = Int64(0)
    _getTime(
      absoluteNanoseconds: nil,
      continuousNanoseconds: nil,
      realtimeSeconds: &realtimeSeconds, 
      realtimeNanoseconds: &realtimeNanoseconds
    )
    return _Date(durationSince1970: 
      .seconds(realtimeSeconds) + .nanoseconds(realtimeNanoseconds))
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension _Date: Equatable {
  public static func == (lhs: _Date, rhs: _Date) -> Bool {
    return lhs.durationSince1970 == rhs.durationSince1970
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension _Date: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(durationSince1970)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension _Date: Comparable {
  public static func < (lhs: _Date, rhs: _Date) -> Bool {
    lhs.durationSince1970 < rhs.durationSince1970
  }
  
  public static func > (lhs: _Date, rhs: _Date) -> Bool {
    lhs.durationSince1970 > rhs.durationSince1970
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension _Date: InstantProtocol {
  public func advanced(by duration: Duration) -> _Date {
    _Date(durationSince1970: durationSince1970 + duration)
  }
  
  public func duration(to other: _Date) -> Duration {
    return other.durationSince1970 - durationSince1970
  }
}