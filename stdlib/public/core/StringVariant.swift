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

@_versioned
internal protocol _StringVariant : RandomAccessCollection
where
  Element == Unicode.UTF16.CodeUnit,
  IndexDistance == Int,
  SubSequence == Self {
  // FIXME associatedtype Encoding : _UnicodeEncoding
  associatedtype CodeUnit : FixedWidthInteger & UnsignedInteger

  var isASCII: Bool { get }

  // Offset-based subscripts allow integer offsets within 0..<count,
  // regardless of what the Index type is.
  subscript(offset: Int) -> Element { get }
  subscript(offsetRange: Range<Int>) -> Self { get }

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

  func _copyToNativeStorage<TargetCodeUnit>(
    of codeUnit: TargetCodeUnit.Type,
    unusedCapacity: Int
  ) -> _SwiftStringStorage<TargetCodeUnit>
}

extension _StringVariant {
  @_inlineable
  @_versioned
  internal func _copyToNativeStorage<TargetCodeUnit>(
    of codeUnit: TargetCodeUnit.Type = TargetCodeUnit.self
  ) -> _SwiftStringStorage<TargetCodeUnit> {
    return _copyToNativeStorage(of: TargetCodeUnit.self, unusedCapacity: 0)
  }

  @_inlineable
  @_versioned
  @inline(__always)
  func _boundsCheck(_ i: Index) {
    _precondition(i >= startIndex && i < endIndex,
      "String index is out of bounds")
  }

  @_inlineable
  @_versioned
  @inline(__always)
  func _boundsCheck(_ range: Range<Index>) {
    _precondition(range.lowerBound >= startIndex,
      "String index range is out of bounds")
    _precondition(range.upperBound <= endIndex,
      "String index range is out of bounds")
  }

  @_inlineable
  @_versioned
  @inline(__always)
  func _boundsCheck(offset i: Int) {
    _precondition(i >= 0 && i < count,
      "String index is out of bounds")
  }

  @_inlineable
  @_versioned
  @inline(__always)
  func _boundsCheck(offsetRange range: Range<Int>) {
    _precondition(range.lowerBound >= 0 && range.upperBound <= count,
      "String index range is out of bounds")
  }

  @_inlineable
  @_versioned
  internal func codeUnit(atCheckedIndex index: Index) -> Element {
    _boundsCheck(index)
    return self[index]
  }

  @_inlineable
  @_versioned
  internal func codeUnit(atCheckedOffset offset: Int) -> Element {
    _boundsCheck(offset: offset)
    return self[offset]
  }

  @_inlineable
  @_versioned
  internal func checkedSlice(_ range: Range<Int>) -> Self {
    _boundsCheck(offsetRange: range)
    return self[range]
  }

  @_inlineable
  @_versioned
  internal func checkedSlice(from startOffset: Int) -> Self {
    let r: Range<Int> = startOffset..<count
    _boundsCheck(offsetRange: r)
    return self[r]
  }

  @_inlineable
  @_versioned
  internal func checkedSlice(upTo endOffset: Int) -> Self {
    let r: Range<Int> = 0..<endOffset
    _boundsCheck(offsetRange: r)
    return self[r]
  }
}
