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

@_exported import CoreMedia // Clang module

extension CMTime {
  public init(seconds: Double, preferredTimescale: CMTimeScale) {
    self = CMTimeMakeWithSeconds(seconds, preferredTimescale: preferredTimescale)
  }

  public init(value: CMTimeValue, timescale: CMTimeScale) {
    self = CMTimeMake(value: value, timescale: timescale)
  }
}

// CMTIME_IS_VALID
// CMTIME_IS_INVALID
// CMTIME_IS_POSITIVEINFINITY
// CMTIME_IS_NEGATIVEINFINITY
// CMTIME_IS_INDEFINITE
// CMTIME_IS_NUMERIC
// CMTIME_HAS_BEEN_ROUNDED
// CMTimeGetSeconds
// CMTimeConvertScale
extension CMTime {
  public var isValid: Bool {
    return self.flags.contains(.valid)
  }

  public var isPositiveInfinity: Bool {
    return self.isValid &&
      self.flags.contains(.positiveInfinity)
  }

  public var isNegativeInfinity: Bool {
    return self.isValid &&
      self.flags.contains(.negativeInfinity)
  }

  public var isIndefinite: Bool {
    return self.isValid &&
      self.flags.contains(.indefinite)
  }

  public var isNumeric: Bool {
    return
      self.flags.intersection([.valid, .impliedValueFlagsMask]) == .valid
  }

  public var hasBeenRounded: Bool {
    return self.isNumeric &&
      self.flags.contains(.hasBeenRounded)
  }

  public var seconds: Double {
    return CMTimeGetSeconds(self)
  }

  public func convertScale(_ newTimescale: Int32, method: CMTimeRoundingMethod)
    -> CMTime {
    return CMTimeConvertScale(self, timescale: newTimescale, method: method)
  }
}

public func CMTIME_IS_VALID(_ time: CMTime) -> Bool {
  return time.isValid
}

public func CMTIME_IS_INVALID(_ time: CMTime) -> Bool {
  return !time.isValid
}

public func CMTIME_IS_POSITIVEINFINITY(_ time: CMTime) -> Bool {
  return time.isPositiveInfinity
}

public func CMTIME_IS_NEGATIVEINFINITY(_ time: CMTime) -> Bool {
  return time.isNegativeInfinity
}

public func CMTIME_IS_INDEFINITE(_ time: CMTime) -> Bool {
  return time.isIndefinite
}

public func CMTIME_IS_NUMERIC(_ time: CMTime) -> Bool {
  return time.isNumeric
}

public func CMTIME_HAS_BEEN_ROUNDED(_ time: CMTime) -> Bool {
  return time.hasBeenRounded
}

extension CMTime {
  // CMTimeAdd
  public static func + (addend1: CMTime, addend2: CMTime) -> CMTime {
    return CMTimeAdd(addend1, addend2)
  }
  
  // CMTimeSubtract
  public static func - (minuend: CMTime, subtrahend: CMTime) -> CMTime {
    return CMTimeSubtract(minuend, subtrahend)
  }
}

// CMTimeCompare
extension CMTime : Equatable, Comparable {
  public static func < (time1: CMTime, time2: CMTime) -> Bool {
    return CMTimeCompare(time1, time2) < 0
  }
  public static func <= (time1: CMTime, time2: CMTime) -> Bool {
    return CMTimeCompare(time1, time2) <= 0
  }
  public static func > (time1: CMTime, time2: CMTime) -> Bool {
    return CMTimeCompare(time1, time2) > 0
  }
  public static func >= (time1: CMTime, time2: CMTime) -> Bool {
    return CMTimeCompare(time1, time2) >= 0
  }
  public static func == (time1: CMTime, time2: CMTime) -> Bool {
    return CMTimeCompare(time1, time2) == 0
  }
  public static func != (time1: CMTime, time2: CMTime) -> Bool {
    return CMTimeCompare(time1, time2) != 0
  }
}
