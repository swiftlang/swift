//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Intents
import Foundation

#if os(iOS)
@available(iOS 10.0, *)
extension INSetSeatSettingsInCarIntent {
    
    @nonobjc
    @available(iOS 12.0, *)
    public convenience init(
        enableHeating: Bool? = nil,
        enableCooling: Bool? = nil,
        enableMassage: Bool? = nil,
        seat: INCarSeat = .unknown,
        level: Int? = nil,
        relativeLevel: INRelativeSetting = .unknown,
        carName: INSpeakableString? = nil
        ) {
        self.init(__enableHeating: enableHeating.map { NSNumber(value: $0) },
                  enableCooling: enableCooling.map { NSNumber(value: $0) },
                  enableMassage: enableMassage.map { NSNumber(value: $0) },
                  seat: seat,
                  level: level.map { NSNumber(value: $0) },
                  relativeLevel: relativeLevel,
                  carName: carName)
    }
    
    @nonobjc
    @available(iOS, obsoleted: 12.0)
    public convenience init(
        enableHeating: Bool? = nil,
        enableCooling: Bool? = nil,
        enableMassage: Bool? = nil,
        seat: INCarSeat = .unknown,
        level: Int? = nil,
        relativeLevel: INRelativeSetting = .unknown
        ) {
        self.init(__enableHeating: enableHeating.map { NSNumber(value: $0) },
                  enableCooling: enableCooling.map { NSNumber(value: $0) },
                  enableMassage: enableMassage.map { NSNumber(value: $0) },
                  seat: seat,
                  level: level.map { NSNumber(value: $0) },
                  relativeLevel: relativeLevel)
    }
    
    @nonobjc
    public final var enableHeating: Bool? {
        return __enableHeating?.boolValue
    }
    
    @nonobjc
    public final var enableCooling: Bool? {
        return __enableCooling?.boolValue
    }
    
    @nonobjc
    public final var enableMassage: Bool? {
        return __enableMassage?.boolValue
    }
    
    @nonobjc
    public final var level: Int? {
        return __level?.intValue
    }
}
#endif
