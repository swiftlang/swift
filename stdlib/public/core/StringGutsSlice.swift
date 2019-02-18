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

  @inlinable
  internal var start: Int {
    @inline(__always) get { return _offsetRange.lowerBound }
  }
  @inlinable
  internal var end: Int {
    @inline(__always) get { return _offsetRange.upperBound }
  }

  @inlinable
  internal var count: Int {
    @inline(__always) get { return _offsetRange.count }
  }

  @inlinable
  internal var isNFCFastUTF8: Bool {
    @inline(__always) get { return _guts.isNFCFastUTF8 }
  }

  @inlinable
  internal var isASCII: Bool {
    @inline(__always) get { return _guts.isASCII }
  }

  @inlinable
  internal var isFastUTF8: Bool {
    @inline(__always) get { return _guts.isFastUTF8 }
  }

  internal var utf8Count: Int {
    @inline(__always) get {
      if _fastPath(self.isFastUTF8) {
        return _offsetRange.count
      }
      return Substring(self).utf8.count
    }
  }

  @inlinable
  internal var range: Range<String.Index> {
    @inline(__always) get {
      return String.Index(encodedOffset: _offsetRange.lowerBound)
         ..< String.Index(encodedOffset: _offsetRange.upperBound)
    }
  }

  @inline(__always)
  internal func withFastUTF8<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    return try _guts.withFastUTF8(range: _offsetRange, f)
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