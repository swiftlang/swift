//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

#if os(iOS)
@available(iOS 10.0, *)
extension INSetSeatTemperatureInCarIntent {
    @nonobjc
    public convenience init(enable: Bool?,
                            seat: INCarSeat,
                            temperatureMode: INTemperatureMode,
                            level: Int?,
                            relativeLevel: INRelativeSetting) {
        self.init(__enable: enable.map { NSNumber(value: $0) },
                  seat: seat,
                  temperatureMode: temperatureMode,
                  level: level.map { NSNumber(value: $0) },
                  relativeLevel: relativeLevel)
    }

    @nonobjc  
    public final var enable: Bool? {
        get {
            return __enable?.boolValue
        }
    }

    @nonobjc  
    public final var level: Int? {
        get {
            return __level?.intValue
        }
    }
}
#endif
