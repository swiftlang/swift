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

// CMTIMERANGE_IS_VALID
// CMTIMERANGE_IS_INVALID
// CMTIMERANGE_IS_INDEFINITE
// CMTIMERANGE_IS_EMPTY
// CMTimeRangeGetEnd
// CMTimeRangeGetUnion
// CMTimeRangeGetIntersection
// CMTimeRangeContainsTime
// CMTimeRangeContainsTimeRange
// CMTimeRangeFromTimeToTime
extension CMTimeRange {
  public init(start: CMTime, end: CMTime) {
    self = CMTimeRangeFromTimeToTime(start, end)
  }

  public var isValid: Bool {
    return self.start.isValid &&
      self.duration.isValid && (self.duration.epoch == 0)
  }

  public var isIndefinite: Bool {
    return self.isValid &&
      (self.start.isIndefinite || self.duration.isIndefinite)
  }

  public var isEmpty: Bool {
    return self.isValid && (self.duration == kCMTimeZero)
  }

  public var end: CMTime {
    return CMTimeRangeGetEnd(self)
  }

  public func union(_ otherRange: CMTimeRange) -> CMTimeRange {
    return CMTimeRangeGetUnion(self, otherRange)
  }
  public func intersection(_ otherRange: CMTimeRange) -> CMTimeRange {
    return CMTimeRangeGetIntersection(self, otherRange)
  }
  public func containsTime(_ time: CMTime) -> Bool {
    return CMTimeRangeContainsTime(self, time)
  }
  public func containsTimeRange(_ range: CMTimeRange) -> Bool {
    return CMTimeRangeContainsTimeRange(self, range)
  }
}

public func CMTIMERANGE_IS_VALID (_ range: CMTimeRange) -> Bool {
  return range.isValid
}
public func CMTIMERANGE_IS_INVALID (_ range: CMTimeRange) -> Bool {
  return !range.isValid
}
public func CMTIMERANGE_IS_INDEFINITE (_ range: CMTimeRange) -> Bool {
  return range.isIndefinite
}
public func CMTIMERANGE_IS_EMPTY (_ range: CMTimeRange) -> Bool {
  return range.isEmpty
}

extension CMTimeRange : Equatable {}

// CMTimeRangeEqual
public func == (range1: CMTimeRange, range2: CMTimeRange) -> Bool {
  return CMTimeRangeEqual(range1, range2)
}

public func != (range1: CMTimeRange, range2: CMTimeRange) -> Bool {
  return !CMTimeRangeEqual(range1, range2)
}

