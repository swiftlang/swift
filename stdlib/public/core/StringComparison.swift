//===----------------------------------------------------------------------===//
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

import SwiftShims


@inlinable @inline(__always) // Fold away opt range and top-level fast-paths
@_effects(readonly)
internal func _stringCompare(
  _ lhs: _StringGuts, _ lhsRange: Range<Int>?,
  _ rhs: _StringGuts, _ rhsRange: Range<Int>?,
  expecting: _StringComparisonResult
) -> Bool {
  _sanityCheck(expecting == .equal || expecting == .less)
  if lhs.rawBits == rhs.rawBits && lhsRange == rhsRange {
    return expecting == .equal
  }
  if _fastPath(lhs.isFastUTF8 && rhs.isFastUTF8) {
    let isNFC = lhs.isNFC && rhs.isNFC
    return lhs.withFastUTF8(range: lhsRange) { utf8Left in
      return rhs.withFastUTF8(range: rhsRange) { utf8Right in
        if isNFC {
          let cmp = _binaryCompare(utf8Left, utf8Right)
          if expecting == .equal {
            return cmp == 0
          }
          _sanityCheck(expecting == .less)
          return cmp < 0
        }

        return _stringCompare(utf8Left, utf8Right, expecting: expecting)
      }
    }
  }
  return _stringCompareSlow(lhs, lhsRange, rhs, rhsRange, expecting: expecting)
}

@usableFromInline
@_effects(readonly)
internal func _stringCompareSlow(
  _ lhs: _StringGuts, _ lhsRange: Range<Int>?,
  _ rhs: _StringGuts, _ rhsRange: Range<Int>?,
  expecting: _StringComparisonResult
) -> Bool {
  return _StringGutsSlice(lhs, lhsRange ?? 0..<lhs.count).compare(
    with: _StringGutsSlice(rhs, rhsRange ?? 0..<rhs.count),
    expecting: expecting)
}

@usableFromInline
@_effects(readonly)
internal func _stringCompare(
  _ left: UnsafeBufferPointer<UInt8>,
  _ right: UnsafeBufferPointer<UInt8>,
  expecting: _StringComparisonResult
) -> Bool {
  _sanityCheck(expecting == .equal || expecting == .less)

  if _binaryCompare(left, right) == 0 {
    return expecting == .equal
  }

  // Do a binary scan finding the point of divergence. From there, see if we can
  // determine our answer right away, otherwise back up to the beginning of the
  // current normalization segment and fall back to the slow path.
  var idx = 0
  let end = Swift.min(left.count, right.count)
  let leftPtr = left.baseAddress._unsafelyUnwrappedUnchecked
  let rightPtr = right.baseAddress._unsafelyUnwrappedUnchecked
  while idx < end {
    guard leftPtr[idx] == rightPtr[idx] else { break }
    idx &+= 1
  }
  _sanityCheck(idx != left.count || idx != right.count,
    "should of been cought by prior binary compare")
  if idx == end {
    // We finished one of our inputs.
    //
    // TODO: This gives us a consistent and good ordering, but technically it
    // could differ from our stated ordering if combination with a prior scalar
    // did not produce a greater-value scalar. Consider checking normality.
    return expecting == _lexicographicalCompare(left.count, right.count)
  }

  // Back up to nearest scalar boundary and check if normal and end of segment.
  // If so, we have our answer.
  while _isContinuation(left[idx]) && idx > 0 { idx &-= 1 }

  // TODO: Refactor this into a function, handle these boundary condition as
  // early returns...
  if !_isContinuation(right[idx]) {
    let (leftScalar, leftLen) = _decodeScalar(left, startingAt: idx)
    let (rightScalar, rightLen) = _decodeScalar(right, startingAt: idx)
    _sanityCheck(leftScalar != rightScalar)

    let nfcQC = leftScalar._isNFCQCYes && rightScalar._isNFCQCYes
    let isSegmentEnd = left.hasNormalizationBoundary(before: idx + leftLen)
                    && right.hasNormalizationBoundary(before: idx + rightLen)
    if _fastPath(nfcQC && isSegmentEnd) {
      return expecting == _lexicographicalCompare(leftScalar, rightScalar)
    }
  }

  // TODO: Back up to start of segment, and slow-path resume from there

  return _StringGutsSlice(_StringGuts(left, isASCII: false)).compare(with:
    _StringGutsSlice(_StringGuts(right, isASCII: false)), expecting: expecting)
}

@_frozen
@usableFromInline
internal enum _StringComparisonResult {
  case less
  case equal
  case greater

  @inlinable @inline(__always)
  internal init(signedNotation int: Int) {
    self = int < 0 ? .less : int == 0 ? .equal : .greater
  }
}

extension _StringGutsSlice {
  @inline(__always)
  @_effects(readonly)
  internal func withNFCCodeUnitsIterator<R>(
    _ f: (_NormalizedUTF8CodeUnitIterator) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    if self.isNFCFastUTF8 {
      return try self.withFastUTF8 {
        return try f(_NormalizedUTF8CodeUnitIterator($0, range: 0..<$0.count))
      }
    }
    if self.isFastUTF8 {
      return try self.withFastUTF8 {
        return try f(_NormalizedUTF8CodeUnitIterator($0, range: 0..<$0.count))
      }
    }
    return try f(_NormalizedUTF8CodeUnitIterator(
      foreign: self._guts, range: self.range))
  }
  @inline(__always)
  @_effects(readonly)
  internal func withNFCCodeUnitsIterator_2<R>(
    _ f: (_NormalizedUTF8CodeUnitIterator_2) throws -> R
  ) rethrows -> R {
    defer { _fixLifetime(self) }
    return try f(_NormalizedUTF8CodeUnitIterator_2(self))
  }
}

// Perform a binary comparison of bytes in memory. Return value is negative if
// less, 0 if equal, positive if greater.
@inlinable @inline(__always) // Memcmp wrapper
internal func _binaryCompare<UInt8>(
  _ lhs: UnsafeBufferPointer<UInt8>, _ rhs: UnsafeBufferPointer<UInt8>
) -> Int {
  var cmp = Int(truncatingIfNeeded:
    _swift_stdlib_memcmp(
      lhs.baseAddress._unsafelyUnwrappedUnchecked,
      rhs.baseAddress._unsafelyUnwrappedUnchecked,
      Swift.min(lhs.count, rhs.count)))
  if cmp == 0 {
    cmp = lhs.count &- rhs.count
  }
  return cmp
}

// Double dispatch functions
extension _StringGutsSlice {
  @usableFromInline
  @_effects(readonly)
  internal func compare(
    with other: _StringGutsSlice, expecting: _StringComparisonResult
  ) -> Bool {
    if self._guts.rawBits == other._guts.rawBits
    && self._offsetRange == other._offsetRange {
      return expecting == .equal
    }

    if _fastPath(self.isNFCFastUTF8 && other.isNFCFastUTF8) {
      Builtin.onFastPath() // aggressively inline / optimize
      return self.withFastUTF8 { nfcSelf in
        return other.withFastUTF8 { nfcOther in
          return expecting == _StringComparisonResult(
            signedNotation: _binaryCompare(nfcSelf, nfcOther))
        }
      }
    }

    if _fastPath(self.isFastUTF8 && other.isFastUTF8) {
      Builtin.onFastPath() // aggressively inline / optimize
      let isEqual = self.withFastUTF8 { utf8Self in
        return other.withFastUTF8 { utf8Other in
          return 0 == _binaryCompare(utf8Self, utf8Other)
        }
      }
      if isEqual { return expecting == .equal }
    }

    return expecting == _slowCompare(with: other)
  }

  @inline(never) // opaque slow-path
  @_effects(readonly)
  internal func _slowCompare(
    with other: _StringGutsSlice
  ) -> _StringComparisonResult {
    return self.withNFCCodeUnitsIterator_2 {
      var selfIter = $0
      return other.withNFCCodeUnitsIterator_2 {
        let otherIter = $0
        return selfIter.compare(with: otherIter)
      }
    }
  }
}

@inline(__always)
internal func _lexicographicalCompare<I: FixedWidthInteger>(
  _ lhs: I, _ rhs: I
) -> _StringComparisonResult {
  return lhs < rhs ? .less : (lhs > rhs ? .greater : .equal)
}
@inline(__always)
internal func _lexicographicalCompare(
  _ lhs: Unicode.Scalar, _ rhs: Unicode.Scalar
) -> _StringComparisonResult {
  return _lexicographicalCompare(lhs.value, rhs.value)
}


