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
extension INSetClimateSettingsInCarIntent {
    @nonobjc
    public convenience init(enableFan: Bool?,
                            enableAirConditioner: Bool?,
                            enableClimateControl: Bool?,
                            enableAutoMode: Bool?,
                            airCirculationMode: INCarAirCirculationMode,
                            fanSpeedIndex: Int?,
                            fanSpeedPercentage: Double?,
                            relativeFanSpeedSetting: INRelativeSetting,
                            temperatureUnit: INTemperatureUnit,
                            temperature: Double?,
                            relativeTemperatureSetting: INRelativeSetting) {
      self.init(__enableFan: enableFan.map { NSNumber(value: $0) },
                enableAirConditioner: enableAirConditioner.map {
                  NSNumber(value: $0)
                },
                enableClimateControl: enableClimateControl.map {
                  NSNumber(value: $0)
                },
                enableAutoMode: enableAutoMode.map { NSNumber(value: $0) },
                airCirculationMode: airCirculationMode,
                fanSpeedIndex: fanSpeedIndex.map { NSNumber(value: $0) },
                fanSpeedPercentage: fanSpeedPercentage.map {
                  NSNumber(value: $0)
                },
                relativeFanSpeedSetting: relativeFanSpeedSetting,
                temperatureUnit: temperatureUnit,
                temperature: temperature.map { NSNumber(value: $0) },
                relativeTemperatureSetting: relativeTemperatureSetting)
    }

    @nonobjc  
    public final var enableFan: Bool? {
        get {
            return __enableFan?.boolValue
        }
    }

    @nonobjc  
    public final var enableAirConditioner: Bool? {
        get {
            return __enableAirConditioner?.boolValue
        }
    }

    @nonobjc  
    public final var enableClimateControl: Bool? {
        get {
            return __enableClimateControl?.boolValue
        }
    }

    @nonobjc  
    public final var enableAutoMode: Bool? {
        get {
            return __enableAutoMode?.boolValue
        }
    }

    @nonobjc  
    public final var fanSpeedIndex: Int? {
        get {
            return __fanSpeedIndex?.intValue
        }
    }

    @nonobjc  
    public final var fanSpeedPercentage: Double? {
        get {
            return __fanSpeedPercentage?.doubleValue
        }
    }

    @nonobjc  
    public final var temperature: Double? {
        get {
            return __temperature?.doubleValue
        }
    }
}
#endif
