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
    self._offsetRange = unsafe Range(_uncheckedBounds: (0, guts.count))
  }

  @inline(__always)
  internal init(_ guts: _StringGuts, _ offsetRange: Range<Int>) {
    _internalInvariant(
      offsetRange.lowerBound >= 0 && offsetRange.upperBound <= guts.count)
    self._guts = guts
    self._offsetRange = offsetRange
  }

  internal var start: Int {
    @inline(__always) get { return _offsetRange.lowerBound }
  }

  internal var end: Int {
    @inline(__always) get { return _offsetRange.upperBound }
  }

  internal var count: Int {
    @inline(__always) get { return _offsetRange.count }
  }

  internal var isNFCFastUTF8: Bool {
    @inline(__always) get { return _guts.isNFCFastUTF8 }
  }

  internal var isASCII: Bool {
    @inline(__always) get { return _guts.isASCII }
  }

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

  internal var range: Range<String.Index> {
    @inline(__always) get {
      let lower = String.Index(_encodedOffset: _offsetRange.lowerBound)
        ._scalarAligned
      let higher = String.Index(_encodedOffset: _offsetRange.upperBound)
        ._scalarAligned
      return unsafe Range(_uncheckedBounds: (lower, higher))
    }
  }

  @inline(__always)
  internal func withFastUTF8<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    return try unsafe _guts.withFastUTF8(range: _offsetRange, f)
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
}
