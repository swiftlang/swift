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


// A sliced _StringGuts, convenient for unifying String/Substring comparison,
// hashing, and RRC.
@_fixed_layout
@usableFromInline
internal struct _StringGutsSlice {
  @usableFromInline
  internal var _guts: _StringGuts

  @usableFromInline
  internal var _offsetRange: Range<Int>

  @inlinable @inline(__always)
  internal init(_ guts: _StringGuts) {
    self._guts = guts
    self._offsetRange = 0..<self._guts.count
  }

  @inlinable @inline(__always)
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

  @inlinable @inline(__always)
  internal func withFastUTF8<R>(
    _ f: (UnsafeBufferPointer<UInt8>) throws -> R
  ) rethrows -> R {
    return try _guts.withFastUTF8(range: _offsetRange, f)
  }

  // Copy UTF-8 contents. Returns number written or nil if not enough space.
  // Contents of the buffer are unspecified if nil is returned.
  @inlinable
  internal func copyUTF8(into mbp: UnsafeMutableBufferPointer<UInt8>) -> Int? {
    let ptr = mbp.baseAddress._unsafelyUnwrappedUnchecked
    if _fastPath(self.isFastUTF8) {
      return self.withFastUTF8 { utf8 in
        guard utf8.count <= mbp.count else { return nil }

        let utf8Start = utf8.baseAddress._unsafelyUnwrappedUnchecked
        ptr.initialize(from: utf8Start, count: utf8.count)
        return utf8.count
      }
    }

    return _foreignCopyUTF8(into: mbp)
  }

  @_effects(releasenone)
  @usableFromInline @inline(never) // slow-path
  internal func _foreignCopyUTF8(
    into mbp: UnsafeMutableBufferPointer<UInt8>
  ) -> Int? {
    var ptr = mbp.baseAddress._unsafelyUnwrappedUnchecked
    var numWritten = 0
    for cu in Substring(self).utf8 {
      guard numWritten < mbp.count else { return nil }
      ptr.initialize(to: cu)
      ptr += 1
      numWritten += 1
    }

    return numWritten
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