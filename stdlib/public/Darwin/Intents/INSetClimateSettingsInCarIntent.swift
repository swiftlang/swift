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
extension INSetClimateSettingsInCarIntent {
    
    @nonobjc
    @available(iOS 12.0, *)
    public convenience init(
        enableFan: Bool? = nil,
        enableAirConditioner: Bool? = nil,
        enableClimateControl: Bool? = nil,
        enableAutoMode: Bool? = nil,
        airCirculationMode: INCarAirCirculationMode = .unknown,
        fanSpeedIndex: Int? = nil,
        fanSpeedPercentage: Double? = nil,
        relativeFanSpeedSetting: INRelativeSetting = .unknown,
        temperature: Measurement<UnitTemperature>? = nil,
        relativeTemperatureSetting: INRelativeSetting = .unknown,
        climateZone: INCarSeat = .unknown,
        carName: INSpeakableString? = nil) {
        self.init(__enableFan: enableFan.map { NSNumber(value: $0) },
                      enableAirConditioner: enableAirConditioner.map { NSNumber(value: $0) },
                      enableClimateControl: enableClimateControl.map { NSNumber(value: $0) },
                      enableAutoMode: enableAutoMode.map { NSNumber(value: $0) },
                      airCirculationMode: airCirculationMode,
                      fanSpeedIndex: fanSpeedIndex.map { NSNumber(value: $0) },
                      fanSpeedPercentage: fanSpeedPercentage.map { NSNumber(value: $0) },
                      relativeFanSpeedSetting: relativeFanSpeedSetting,
                      temperature: temperature,
                      relativeTemperatureSetting: relativeTemperatureSetting,
                      climateZone: climateZone,
                      carName: carName)
    }
    
    @nonobjc
    @available(iOS, obsoleted: 12.0)
    public convenience init(
        enableFan: Bool? = nil,
        enableAirConditioner: Bool? = nil,
        enableClimateControl: Bool? = nil,
        enableAutoMode: Bool? = nil,
        airCirculationMode: INCarAirCirculationMode = .unknown,
        fanSpeedIndex: Int? = nil,
        fanSpeedPercentage: Double? = nil,
        relativeFanSpeedSetting: INRelativeSetting = .unknown,
        temperature: Measurement<UnitTemperature>? = nil,
        relativeTemperatureSetting: INRelativeSetting = .unknown,
        climateZone: INCarSeat = .unknown) {
        self.init(__enableFan: enableFan.map { NSNumber(value: $0) },
                  enableAirConditioner: enableAirConditioner.map { NSNumber(value: $0) },
                  enableClimateControl: enableClimateControl.map { NSNumber(value: $0) },
                  enableAutoMode: enableAutoMode.map { NSNumber(value: $0) },
                  airCirculationMode: airCirculationMode,
                  fanSpeedIndex: fanSpeedIndex.map { NSNumber(value: $0) },
                  fanSpeedPercentage: fanSpeedPercentage.map { NSNumber(value: $0) },
                  relativeFanSpeedSetting: relativeFanSpeedSetting,
                  temperature: temperature,
                  relativeTemperatureSetting: relativeTemperatureSetting,
                  climateZone: climateZone)
    }
    
    @nonobjc
    public final var enableFan: Bool? {
        return __enableFan?.boolValue
    }
    
    @nonobjc
    public final var enableAirConditioner: Bool? {
        return __enableAirConditioner?.boolValue
    }
    
    @nonobjc
    public final var enableClimateControl: Bool? {
        return __enableClimateControl?.boolValue
    }
    
    @nonobjc
    public final var enableAutoMode: Bool? {
        return __enableAutoMode?.boolValue
    }
    
    @nonobjc
    public final var fanSpeedIndex: Int? {
        return __fanSpeedIndex?.intValue
    }
    
    @nonobjc
    public final var fanSpeedPercentage: Double? {
        return __fanSpeedPercentage?.doubleValue
    }
}
#endif
