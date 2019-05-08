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

// TODO(String performance): Unfortunately, this slice struct seems to add
// overhead. We may want to wean ourselves off of this and have all users just
// also store a range.

// A sliced _StringGuts, convenient for unifying String/Substring comparison,
// hashing, and RRC.
internal struct _StringGutsSlice {
  internal var _guts: _StringGuts

  internal var _offsetRange: Range<Int>

  @inline(__always)
  internal init(_ guts: _StringGuts) {
    self._guts = guts
    self._offsetRange = 0..<self._guts.count
  }

  @inline(__always)
  internal init(_ guts: _StringGuts, _ offsetRange: Range<Int>) {
    self._guts = guts
    self._offsetRange = offsetRange
  }

  @inlinable @inline(__always)
  internal var start: Int { _offsetRange.lowerBound }

  @inlinable @inline(__always)
  internal var end: Int { _offsetRange.upperBound }

  @inlinable @inline(__always)
  internal var count: Int { _offsetRange.count }

  @inlinable @inline(__always)
  internal var isNFCFastUTF8: Bool { _guts.isNFCFastUTF8 }

  @inlinable @inline(__always)
  internal var isASCII: Bool { _guts.isASCII }

  @inlinable @inline(__always)
  internal var isFastUTF8: Bool { _guts.isFastUTF8 }

  @inline(__always)
  internal var utf8Count: Int {
    if _fastPath(self.isFastUTF8) {
      return _offsetRange.count
    }
    return Substring(self).utf8.count
  }

  @inlinable @inline(__always)
  internal var range: Range<String.Index> {
    String.Index(_encodedOffset: _offsetRange.lowerBound)
      ..< String.Index(_encodedOffset: _offsetRange.upperBound)
  }

  @inline(__always)
  internal func withFastUTF8<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    try _guts.withFastUTF8(range: _offsetRange, f)
  }

  @_effects(releasenone)
  internal func foreignErrorCorrectedScalar(
    startingAt idx: String.Index
  ) -> (Unicode.Scalar, scalarLength: Int) {
    let (scalar, len) = _guts.foreignErrorCorrectedScalar(startingAt: idx)
    if _slowPath(idx.encoded(offsetBy: len) > range.upperBound) {
      return (Unicode.Scalar._replacementCharacter, 1)
    }
    return (scalar, len)
  }

  internal func foreignHasNormalizationBoundary(
    before index: String.Index
  ) -> Bool {
    if index == range.lowerBound || index == range.upperBound {
      return true
    }
    return _guts.foreignHasNormalizationBoundary(before: index)
  }
}