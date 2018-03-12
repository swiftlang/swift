//===--- PastEndIndex.swift -----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// FIXME: docs
@_frozen
public enum PastEndIndex<Bound> {
  case pastEnd
  case inRange(Bound)
}

extension PastEndIndex: Equatable where Bound: Equatable {
  @inlinable
  public static func == (lhs: PastEndIndex, rhs: PastEndIndex) -> Bool {
    switch (lhs, rhs) {
    case (.inRange(let l), .inRange(let r)):
      return l == r
    case (.pastEnd, .pastEnd):
      return true
    default:
      return false
    }
  }
}

extension PastEndIndex: Comparable where Bound: Comparable {
  @inlinable
  public static func < (lhs: PastEndIndex, rhs: PastEndIndex) -> Bool {
    switch (lhs, rhs) {
    case (.inRange(let l), .inRange(let r)):
      return l < r
    case (.inRange(_), .pastEnd):
      return true
    default:
      return false
    }
  }
}

extension PastEndIndex: Hashable where Bound: Hashable {
  public var hashValue: Int {
    switch self {
    case .inRange(let value):
      return value.hashValue
    case .pastEnd:
      return .max
    }
  }
}
