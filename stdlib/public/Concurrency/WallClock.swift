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
public struct WallClock { }

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension ClockProtocol where Self == WallClock {
  public static var wall: WallClock { return WallClock() }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, macCatalyst 9999, *)
extension WallClock: ClockProtocol {
  public var now: _Date {
    _Date.now
  }
  
  public static var now: _Date {
    _Date.now
  }
  
  public func sleep(until deadline: _Date) async throws {
    if deadline.durationSince1970 > .zero {
      try await Task.sleep(until: 
        UInt64(deadline.durationSince1970.seconds), 
        UInt64(deadline.durationSince1970.nanoseconds), 
        clockId: .wall)
    }
  }
}
