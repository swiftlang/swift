//===--- StringVariant.swift - Common operations on String storage views -===//
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

@usableFromInline
internal protocol _StringVariant : RandomAccessCollection
where
  Element == Unicode.UTF16.CodeUnit,
  SubSequence == Self {
  // FIXME associatedtype Encoding : _UnicodeEncoding
  associatedtype CodeUnit : FixedWidthInteger & UnsignedInteger
  associatedtype UnicodeScalarIterator : IteratorProtocol
    where UnicodeScalarIterator.Element == Unicode.Scalar

  var isASCII: Bool { get }

  // Offset-based subscripts allow integer offsets within 0..<count,
  // regardless of what the Index type is.
  subscript(offset: Int) -> Element { get }
  subscript(offsetRange: Range<Int>) -> Self { get }

  func makeUnicodeScalarIterator() -> UnicodeScalarIterator

  // Measure the length in UTF-16 code units of the first extended grapheme
  // cluster in self.
  func measureFirstExtendedGraphemeCluster() -> Int

  // Measure the length in UTF-16 code units of the last extended grapheme
  // cluster in self.
  func measureLastExtendedGraphemeCluster() -> Int

  // Slow path for measuring the length in UTF-16 code units of the first
  // extended grapheme cluster in self.
  func _measureFirstExtendedGraphemeClusterSlow() -> Int

  // Slow path for measuring the length in UTF-16 code units of the last
  // extended grapheme cluster in self.
  func _measureLastExtendedGraphemeClusterSlow() -> Int

  func _copy<TargetCodeUnit>(
    into target: UnsafeMutableBufferPointer<TargetCodeUnit>
  ) where TargetCodeUnit : FixedWidthInteger & UnsignedInteger
}

extension _StringVariant {
  @inlinable // FIXME(sil-serialize-all)
  internal func _copyToNativeStorage<TargetCodeUnit>(
    of codeUnit: TargetCodeUnit.Type = TargetCodeUnit.self,
    unusedCapacity: Int = 0
  ) -> _SwiftStringStorage<TargetCodeUnit>
  where TargetCodeUnit : FixedWidthInteger & UnsignedInteger {
    let storage = _SwiftStringStorage<TargetCodeUnit>.create(
      capacity: count + unusedCapacity,
      count: count)
    _copy(into: storage.usedBuffer)
    return storage
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(_ i: Index) {
    _precondition(i >= startIndex && i < endIndex,
      "String index is out of bounds")
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(_ range: Range<Index>) {
    _precondition(range.lowerBound >= startIndex,
      "String index range is out of bounds")
    _precondition(range.upperBound <= endIndex,
      "String index range is out of bounds")
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(offset i: Int) {
    _precondition(i >= 0 && i < count,
      "String index is out of bounds")
  }

  @inlinable
  @inline(__always)
  func _boundsCheck(offsetRange range: Range<Int>) {
    _precondition(range.lowerBound >= 0 && range.upperBound <= count,
      "String index range is out of bounds")
  }

  @inlinable
  internal func codeUnit(atCheckedIndex index: Index) -> Element {
    _boundsCheck(index)
    return self[index]
  }

  @inlinable
  internal func codeUnit(atCheckedOffset offset: Int) -> Element {
    _boundsCheck(offset: offset)
    return self[offset]
  }

  @inlinable
  internal func checkedSlice(_ range: Range<Int>) -> Self {
    _boundsCheck(offsetRange: range)
    return self[range]
  }

  @inlinable
  internal func checkedSlice(from startOffset: Int) -> Self {
    let r: Range<Int> = startOffset..<count
    _boundsCheck(offsetRange: r)
    return self[r]
  }

  @inlinable
  internal func checkedSlice(upTo endOffset: Int) -> Self {
    let r: Range<Int> = 0..<endOffset
    _boundsCheck(offsetRange: r)
    return self[r]
  }
}

extension _StringVariant {
  @inlinable
  internal func unicodeScalarWidth(startingAt offset: Int) -> Int {
    _boundsCheck(offset: offset)
    if _slowPath(UTF16.isLeadSurrogate(self[offset])) {
      if offset + 1 < self.count &&
      UTF16.isTrailSurrogate(self[offset + 1]) {
        return 2
      }
    }
    return 1
  }

  @inlinable
  func unicodeScalarWidth(endingAt offset: Int) -> Int {
    _boundsCheck(offset: offset - 1)
    if _slowPath(UTF16.isTrailSurrogate(self[offset - 1])) {
      if offset >= 2 && UTF16.isLeadSurrogate(self[offset - 2]) {
        return 2
      }
    }
    return 1
  }

  @inlinable
  func unicodeScalar(startingAt offset: Int) -> Unicode.Scalar {
    let u0 = self.codeUnit(atCheckedOffset: offset)
    if _fastPath(UTF16._isScalar(u0)) {
      return Unicode.Scalar(_unchecked: UInt32(u0))
    }
    if UTF16.isLeadSurrogate(u0) && offset + 1 < count {
      let u1 = self[offset + 1]
      if UTF16.isTrailSurrogate(u1) {
        return UTF16._decodeSurrogates(u0, u1)
      }
    }
    return Unicode.Scalar._replacementCharacter
  }

  @inlinable
  func unicodeScalar(endingAt offset: Int) -> Unicode.Scalar {
    let u1 = self.codeUnit(atCheckedOffset: offset - 1)
    if _fastPath(UTF16._isScalar(u1)) {
      return Unicode.Scalar(_unchecked: UInt32(u1))
    }
    if UTF16.isTrailSurrogate(u1) && offset >= 2 {
      let u0 = self[offset - 2]
      if UTF16.isLeadSurrogate(u0) {
        return UTF16._decodeSurrogates(u0, u1)
      }
    }
    return Unicode.Scalar._replacementCharacter
  }
}
