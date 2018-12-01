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
extension INSetRadioStationIntent {
  @nonobjc
  public convenience init(
    radioType: INRadioType = .unknown,
    frequency: Double? = nil,
    stationName: String? = nil,
    channel: String? = nil,
    presetNumber: Int? = nil
  ) {
    self.init(__radioType: radioType,
      frequency: frequency.map { NSNumber(value: $0) },
      stationName: stationName,
      channel: channel,
      presetNumber: presetNumber.map { NSNumber(value: $0) })
  }

  @nonobjc
  public final var frequency: Double? {
    return __frequency?.doubleValue
  }

  @nonobjc
  public final var presetNumber: Int? {
    return __presetNumber?.intValue
  }

}

#endif
