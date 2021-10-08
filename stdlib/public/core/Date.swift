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

@available(macOS 10.9, iOS 7.0, tvOS 9.0, watchOS 2.0, macCatalyst 13.0, *)
@_originallyDefinedIn(module: "Foundation", 
  macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999)
public struct Date {
  public var durationSince1970: Duration
  
  public init(durationSince1970: Duration) {
    self.durationSince1970 = durationSince1970
  }

  @available(macOS 12, iOS 15, tvOS 15, watchOS 8, *)
  public static var now: Date { 
    var realtimeSeconds = Int64(0)
    var realtimeNanoseconds = Int64(0)
    _getTime(
      absoluteNanoseconds: nil,
      continuousNanoseconds: nil,
      realtimeSeconds: &realtimeSeconds, 
      realtimeNanoseconds: &realtimeNanoseconds
    )
    return Date(durationSince1970: 
      .seconds(realtimeSeconds) + .nanoseconds(realtimeNanoseconds))
  }
}

extension Date: Equatable {
  public static func == (lhs: Date, rhs: Date) -> Bool {
    return lhs.durationSince1970 == rhs.durationSince1970
  }
}

extension Date: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(durationSince1970)
  }
}

extension Date: Comparable {
  public static func < (lhs: Date, rhs: Date) -> Bool {
    lhs.durationSince1970 < rhs.durationSince1970
  }
  
  public static func > (lhs: Date, rhs: Date) -> Bool {
    lhs.durationSince1970 > rhs.durationSince1970
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension Date: InstantProtocol {
  public func advanced(by duration: Duration) -> Date {
    Date(durationSince1970: durationSince1970 + duration)
  }
  
  public func duration(to other: Date) -> Duration {
    return other.durationSince1970 - durationSince1970
  }
}