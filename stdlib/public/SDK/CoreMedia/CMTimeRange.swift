//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@exported import CoreMedia // Clang module

// CMTIMERANGE_IS_VALID
// CMTIMERANGE_IS_INVALID
// CMTIMERANGE_IS_INDEFINITE
// CMTIMERANGE_IS_EMPTY
// CMTimeRangeGetEnd
// CMTimeRangeGetUnion
// CMTimeRangeGetIntersection
// CMTimeRangeContainsTime
// CMTimeRangeContainsTimeRange
extension CMTimeRange {
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

  public func union(otherRange: CMTimeRange) -> CMTimeRange {
    return CMTimeRangeGetUnion(self, otherRange)
  }
  public func intersection(otherRange: CMTimeRange) -> CMTimeRange {
    return CMTimeRangeGetIntersection(self, otherRange)
  }
  public func containsTime(time: CMTime) -> Bool {
    return CMTimeRangeContainsTime(self, time) != 0
  }
  public func containsTimeRange(range: CMTimeRange) -> Bool {
    return CMTimeRangeContainsTimeRange(self, range) != 0
  }
}

public func CMTIMERANGE_IS_VALID (range: CMTimeRange) -> Bool {
  return range.isValid
}
public func CMTIMERANGE_IS_INVALID (range: CMTimeRange) -> Bool {
  return !range.isValid
}
public func CMTIMERANGE_IS_INDEFINITE (range: CMTimeRange) -> Bool {
  return range.isIndefinite
}
public func CMTIMERANGE_IS_EMPTY (range: CMTimeRange) -> Bool {
  return range.isEmpty
}

// CMTimeRangeEqual
public func == (range1: CMTimeRange, range2: CMTimeRange) -> Bool {
  return CMTimeRangeEqual(range1, range2) != 0
}

public func != (range1: CMTimeRange, range2: CMTimeRange) -> Bool {
  return CMTimeRangeEqual(range1, range2) == 0
}

