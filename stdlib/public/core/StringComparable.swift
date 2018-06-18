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

import SwiftShims

extension _StringGuts {
  @inline(__always)
  @inlinable
  public func _bitwiseEqualTo(_ other: _StringGuts) -> Bool {
    return self.rawBits == other.rawBits
  }

  @inlinable
  internal static func isEqual(
    _ left: _StringGuts, to right: _StringGuts
  ) -> Bool {
    // Bitwise equality implies string equality
    if left._bitwiseEqualTo(right) {
      return true
    }
    if left._isSmall && right._isSmall {
      // TODO: Ensure normality when adding UTF-8 support
      _sanityCheck(left._isASCIIOrSmallASCII && right._isASCIIOrSmallASCII,
        "Need to ensure normality")

      // Equal small strings should be bitwise equal if ASCII
      return false
    }
    return compare(left, to: right) == 0
  }

  @inlinable
  internal static func isEqual(
    _ left: _StringGuts, _ leftRange: Range<Int>,
    to right: _StringGuts, _ rightRange: Range<Int>
  ) -> Bool {
    // Bitwise equality implies string equality
    if left._bitwiseEqualTo(right) && leftRange == rightRange {
      return true
    }
    return compare(left, leftRange, to: right, rightRange) == 0
  }

  @inlinable
  internal static func isLess(
    _ left: _StringGuts, than right: _StringGuts
  ) -> Bool {
    // Bitwise equality implies string equality
    if left._bitwiseEqualTo(right) {
      return false
    }
    if left._isSmall && right._isSmall {
      // Small strings compare lexicographically if ASCII
      return left._smallUTF8String._compare(right._smallUTF8String) == .less
    }
    return compare(left, to: right) == -1
  }

  @inlinable
  internal static func isLess(
    _ left: _StringGuts, _ leftRange: Range<Int>,
    than right: _StringGuts, _ rightRange: Range<Int>
  ) -> Bool {
    // Bitwise equality implies string equality
    if left._bitwiseEqualTo(right) && leftRange == rightRange {
      return false
    }
    return compare(left, leftRange, to: right, rightRange) == -1
  }

  @inlinable
  internal static func compare(
    _ left: _StringGuts, _ leftRange: Range<Int>,
    to right: _StringGuts, _ rightRange: Range<Int>
  ) -> Int {
    defer { _fixLifetime(left) }
    defer { _fixLifetime(right) }

    if left.isASCII && right.isASCII {
      let leftASCII = left._unmanagedASCIIView[leftRange]
      let rightASCII = right._unmanagedASCIIView[rightRange]
      let result = leftASCII.compareASCII(to: rightASCII)
      return result
    }

    let leftBits = left.rawBits
    let rightBits = right.rawBits

    return _compareUnicode(leftBits, leftRange, rightBits, rightRange)
  }

  @inlinable
  internal static func compare(
    _ left: _StringGuts, to right: _StringGuts
  ) -> Int {
    defer { _fixLifetime(left) }
    defer { _fixLifetime(right) }

    if left.isASCII && right.isASCII {
      let leftASCII = left._unmanagedASCIIView
      let rightASCII = right._unmanagedASCIIView
      let result = leftASCII.compareASCII(to: rightASCII)
      return result
    }

    let leftBits = left.rawBits
    let rightBits = right.rawBits

    return _compareUnicode(leftBits, rightBits)
  }
}

extension StringProtocol {
  @inlinable // FIXME(sil-serialize-all)
  public static func ==<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
    return _StringGuts.isEqual(
      lhs._wholeString._guts, lhs._encodedOffsetRange,
      to: rhs._wholeString._guts, rhs._encodedOffsetRange)
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func !=<S: StringProtocol>(lhs: Self, rhs: S) -> Bool {
    return !(lhs == rhs)
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func < <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return _StringGuts.isLess(
      lhs._wholeString._guts, lhs._encodedOffsetRange,
      than: rhs._wholeString._guts, rhs._encodedOffsetRange)
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func > <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return rhs < lhs
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func <= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return !(rhs < lhs)
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func >= <R: StringProtocol>(lhs: Self, rhs: R) -> Bool {
    return !(lhs < rhs)
  }
}

extension String : Equatable {
  // FIXME: Why do I need this? If I drop it, I get "ambiguous use of operator"
  @inlinable // FIXME(sil-serialize-all)
  public static func ==(lhs: String, rhs: String) -> Bool {
    return _StringGuts.isEqual(lhs._guts, to: rhs._guts)
  }
}

extension String : Comparable {
  // FIXME: Why do I need this? If I drop it, I get "ambiguous use of operator"
  @inlinable // FIXME(sil-serialize-all)
  public static func < (lhs: String, rhs: String) -> Bool {
    return _StringGuts.isLess(lhs._guts, than: rhs._guts)
  }
}

extension Substring : Equatable {}
