//===--- RangeSelection.swift ---------------------------------------------===//
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

import StdlibUnittest

public enum RangeSelection {
  case emptyRange
  case leftEdge
  case rightEdge
  case middle
  case leftHalf
  case rightHalf
  case full
  case offsets(Int, Int)

  public var isEmpty: Bool {
    switch self {
    case .emptyRange: return true
    default: return false
    }
  }

  public func range<C : Collection>(in c: C) -> Range<C.Index> {
    switch self {
      case .emptyRange: return c.endIndex..<c.endIndex
      case .leftEdge: return c.startIndex..<c.startIndex
      case .rightEdge: return c.endIndex..<c.endIndex
      case .middle:
        let start = c.index(c.startIndex, offsetBy: c.count / 4)
        let end = c.index(c.startIndex, offsetBy: 3 * c.count / 4 + 1)
        return start..<end
      case .leftHalf:
        let start = c.startIndex
        let end = c.index(start, offsetBy: c.count / 2)
        return start..<end
      case .rightHalf:
        let start = c.index(c.startIndex, offsetBy: c.count / 2)
        let end = c.endIndex
        return start..<end
      case .full:
        return c.startIndex..<c.endIndex
      case let .offsets(lowerBound, upperBound):
        let start = c.index(c.startIndex, offsetBy: numericCast(lowerBound))
        let end = c.index(c.startIndex, offsetBy: numericCast(upperBound))
        return start..<end
    }
  }

  public func countableRange<C : Collection>(in c: C) -> CountableRange<C.Index> {
    return CountableRange(range(in: c))
  }

  public func closedRange<C : Collection>(in c: C) -> ClosedRange<C.Index> {
    switch self {
      case .emptyRange: fatalError("Closed range cannot be empty")
      case .leftEdge: return c.startIndex...c.startIndex
      case .rightEdge:
        let beforeEnd = c.index(c.startIndex, offsetBy: c.count - 1)
        return beforeEnd...beforeEnd
      case .middle:
        let start = c.index(c.startIndex, offsetBy: c.count / 4)
        let end = c.index(c.startIndex, offsetBy: 3 * c.count / 4)
        return start...end
      case .leftHalf:
        let start = c.startIndex
        let end = c.index(start, offsetBy: c.count / 2 - 1)
        return start...end
      case .rightHalf:
        let start = c.index(c.startIndex, offsetBy: c.count / 2)
        let beforeEnd = c.index(c.startIndex, offsetBy: c.count - 1)
        return start...beforeEnd
      case .full:
        let beforeEnd = c.index(c.startIndex, offsetBy: c.count - 1)
        return c.startIndex...beforeEnd
      case let .offsets(lowerBound, upperBound):
        let start = c.index(c.startIndex, offsetBy: numericCast(lowerBound))
        let end = c.index(c.startIndex, offsetBy: numericCast(upperBound))
        return start...end
    }
  }

  public func countableClosedRange<C : Collection>(in c: C) -> CountableClosedRange<C.Index> {
    return CountableClosedRange(closedRange(in: c))
  }
}
