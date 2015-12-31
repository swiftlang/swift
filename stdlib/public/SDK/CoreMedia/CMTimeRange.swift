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

  @warn_unused_result
  public func union(otherRange: CMTimeRange) -> CMTimeRange {
    return CMTimeRangeGetUnion(self, otherRange)
  }
  @warn_unused_result
  public func intersection(otherRange: CMTimeRange) -> CMTimeRange {
    return CMTimeRangeGetIntersection(self, otherRange)
  }
  @warn_unused_result
  public func containsTime(time: CMTime) -> Bool {
    return CMTimeRangeContainsTime(self, time).boolValue
  }
  @warn_unused_result
  public func containsTimeRange(range: CMTimeRange) -> Bool {
    return CMTimeRangeContainsTimeRange(self, range).boolValue
  }
}

@warn_unused_result
public func CMTIMERANGE_IS_VALID (range: CMTimeRange) -> Bool {
  return range.isValid
}
@warn_unused_result
public func CMTIMERANGE_IS_INVALID (range: CMTimeRange) -> Bool {
  return !range.isValid
}
@warn_unused_result
public func CMTIMERANGE_IS_INDEFINITE (range: CMTimeRange) -> Bool {
  return range.isIndefinite
}
@warn_unused_result
public func CMTIMERANGE_IS_EMPTY (range: CMTimeRange) -> Bool {
  return range.isEmpty
}

extension CMTimeRange : Equatable {}

// CMTimeRangeEqual
@warn_unused_result
public func == (range1: CMTimeRange, range2: CMTimeRange) -> Bool {
  return CMTimeRangeEqual(range1, range2).boolValue
}

@warn_unused_result
public func != (range1: CMTimeRange, range2: CMTimeRange) -> Bool {
  return !CMTimeRangeEqual(range1, range2).boolValue
}

