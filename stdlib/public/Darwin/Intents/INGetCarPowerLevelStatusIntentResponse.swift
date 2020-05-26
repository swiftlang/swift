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

#if os(iOS) || os(watchOS)
@available(iOS 10.3, watchOS 3.2, *)
extension INGetCarPowerLevelStatusIntentResponse {
    @nonobjc
    public final var fuelPercentRemaining: Float? {
        get {
            return __fuelPercentRemaining?.floatValue
        }
        set(newPercent) {
            __fuelPercentRemaining = newPercent.map { NSNumber(value: $0) }
        }
    }

    @nonobjc
    public final var chargePercentRemaining: Float? {
        get {
            return __chargePercentRemaining?.floatValue
        }
        set(newPercent) {
            __chargePercentRemaining = newPercent.map { NSNumber(value: $0) }
        }
    }

    @nonobjc
    @available(iOS 12.0, watchOS 5.0, *)
    public final var charging: Bool? {
        get {
            return __charging?.boolValue
        }
        set(newCharging) {
            __charging = newCharging.map { NSNumber(value: $0) }
        }
    }

    @nonobjc
    @available(iOS 12.0, watchOS 5.0, *)
    public final var minutesToFull: Int? {
        get {
            return __minutesToFull?.intValue
        }
        set(newMinutesToFull) {
            __minutesToFull = newMinutesToFull.map { NSNumber(value: $0) }
        }
    }
}
#endif
