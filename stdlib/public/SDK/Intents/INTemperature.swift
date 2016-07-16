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
extension INTemperature {
  @nonobjc
  public convenience init(value: Double, unit: UnitTemperature) {
    self.init(__unit: unit, value: NSNumber(value: value))
  }

  @nonobjc
  public convenience init(_ measurement: Measurement<UnitTemperature>) {
    self.init(__unit: measurement.unit,
      value: NSNumber(value: measurement.value))
  }

  @nonobjc
  public final var value: Double? {
    return __value?.doubleValue
  }
}
#endif
