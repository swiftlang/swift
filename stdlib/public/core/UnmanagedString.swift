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

internal typealias _UnmanagedASCIIString = _UnmanagedString<UInt8>
internal typealias _UnmanagedUTF16String = _UnmanagedString<UTF16.CodeUnit>

@inlinable
internal
func memcpy_zext<
  Target: FixedWidthInteger & UnsignedInteger,
  Source: FixedWidthInteger & UnsignedInteger
>(
  dst: UnsafeMutablePointer<Target>, src: UnsafePointer<Source>, count: Int
) {
  _sanityCheck(Source.bitWidth < Target.bitWidth)
  for i in 0..<count {
    dst[i] = Target(src[i])
  }
}

@inlinable
internal
func memcpy_trunc<
  Target: FixedWidthInteger & UnsignedInteger,
  Source: FixedWidthInteger & UnsignedInteger
>(
  dst: UnsafeMutablePointer<Target>, src: UnsafePointer<Source>, count: Int
) {
  _sanityCheck(Source.bitWidth > Target.bitWidth)
  for i in 0..<count {
    dst[i] = Target(truncatingIfNeeded: src[i])
  }
}

@_fixed_layout
@usableFromInline
internal
struct _UnmanagedString<CodeUnit>
  where CodeUnit : FixedWidthInteger & UnsignedInteger {
  // TODO: Use the extra 13 bits
  //
  // StringGuts when representing UnmanagedStrings should have an extra 13 bits
  // *at least* to store whatever we want, e.g. flags. x86_64 ABI has at least
  // 13 bits due to:
  //   * 8 bits from count: 56-bit (max) address spaces means we need at most
  //     56-bit count
  //   * 5 bits from BridgeObject: 64 - 2 tagging - 56-bit address space - 1 bit
  //     designating UnsafeString
  //

  @usableFromInline
  internal var start: UnsafePointer<CodeUnit>

  @usableFromInline
  internal var count: Int

  @inlinable
  init(start: UnsafePointer<CodeUnit>, count: Int) {
    _sanityCheck(CodeUnit.self == UInt8.self || CodeUnit.self == UInt16.self)
    self.start = start
    self.count = count
  }

  @inlinable
  init(_ bufPtr: UnsafeBufferPointer<CodeUnit>) {
    self.init(
      start: bufPtr.baseAddress._unsafelyUnwrappedUnchecked,
      count: bufPtr.count)
  }
}

extension _UnmanagedString {
  @inlinable
  internal var end: UnsafePointer<CodeUnit> {
    return start + count
  }

  @inlinable
  internal var rawStart: UnsafeRawPointer {
    return UnsafeRawPointer(start)
  }

  @inlinable
  internal var rawEnd: UnsafeRawPointer {
    return UnsafeRawPointer(end)
  }

  @inlinable
  internal var buffer: UnsafeBufferPointer<CodeUnit> {
    return .init(start: start, count: count)
  }

  @inlinable
  internal var rawBuffer: UnsafeRawBufferPointer {
    return .init(start: rawStart, count: rawEnd - rawStart)
  }
}

extension _UnmanagedString : RandomAccessCollection {
  internal typealias Element = UTF16.CodeUnit
  // Note that the Index type can't be an integer offset because Collection
  // requires that SubSequence share indices with the original collection.
  // Therefore, we use pointers as the index type; however, we also provide
  // integer subscripts as a convenience, in a separate extension below.
  internal typealias Index = UnsafePointer<CodeUnit>
  internal typealias IndexDistance = Int
  internal typealias Indices = Range<Index>
  internal typealias SubSequence = _UnmanagedString

  @inlinable
  internal
  var startIndex: Index { return start }

  @inlinable
  internal
  var endIndex: Index { return end }

  @inlinable
  internal subscript(position: Index) -> UTF16.CodeUnit {
    @inline(__always)
    get {
      _sanityCheck(position >= start && position < end)
      return UTF16.CodeUnit(position.pointee)
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(_ bounds: Range<Index>) -> SubSequence {
    _sanityCheck(bounds.lowerBound >= start && bounds.upperBound <= end)
    return _UnmanagedString(start: bounds.lowerBound, count: bounds.count)
  }
}

extension _UnmanagedString : _StringVariant {
  @inlinable
  internal var isASCII: Bool {
    // NOTE: For now, single byte means ASCII. Might change in future
    return CodeUnit.bitWidth == 8
  }

  @inlinable
  internal subscript(offset: Int) -> UTF16.CodeUnit {
    @inline(__always)
    get {
      _sanityCheck(offset >= 0 && offset < count)
      return UTF16.CodeUnit(start[offset])
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  internal subscript(offsetRange: Range<Int>) -> _UnmanagedString {
    _sanityCheck(offsetRange.lowerBound >= 0 && offsetRange.upperBound <= count)
    return _UnmanagedString(
      start: start + offsetRange.lowerBound,
      count: offsetRange.count)
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal subscript(offsetRange: PartialRangeFrom<Int>) -> SubSequence {
    _sanityCheck(offsetRange.lowerBound >= 0)
    return _UnmanagedString(
      start: start + offsetRange.lowerBound, 
      count: self.count - offsetRange.lowerBound
    )
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal subscript(offsetRange: PartialRangeUpTo<Int>) -> SubSequence {
    _sanityCheck(offsetRange.upperBound <= count)
    return _UnmanagedString(
      start: start, 
      count: offsetRange.upperBound
    )
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal subscript(offsetRange: PartialRangeThrough<Int>) -> SubSequence {
    _sanityCheck(offsetRange.upperBound < count)
    return _UnmanagedString(
      start: start, 
      count: offsetRange.upperBound + 1
    )
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _copy<TargetCodeUnit>(
    into target: UnsafeMutableBufferPointer<TargetCodeUnit>
  ) where TargetCodeUnit : FixedWidthInteger & UnsignedInteger {
    _sanityCheck(
      TargetCodeUnit.self == UInt8.self || TargetCodeUnit.self == UInt16.self)
    guard count > 0 else { return }
    _sanityCheck(target.count >= self.count)
    if CodeUnit.bitWidth == TargetCodeUnit.bitWidth {
      _memcpy(
        dest: target.baseAddress!,
        src: self.start,
        size: UInt(self.count * MemoryLayout<CodeUnit>.stride))
    } else if CodeUnit.bitWidth == 8 {
      _sanityCheck(TargetCodeUnit.bitWidth == 16)
      memcpy_zext(
        dst: target.baseAddress._unsafelyUnwrappedUnchecked,
        src: start,
        count: self.count)
    } else {
      _sanityCheck(CodeUnit.bitWidth == 16 && TargetCodeUnit.bitWidth == 8)
      _sanityCheck(self.filter { $0 >= UInt8.max }.isEmpty, "ASCII only")
      memcpy_trunc(
        dst: target.baseAddress._unsafelyUnwrappedUnchecked,
        src: start,
        count: self.count)
    }
  }

  @_fixed_layout
  @usableFromInline // FIXME(sil-serialize-all)
  internal struct UnicodeScalarIterator : IteratorProtocol {
    @usableFromInline // FIXME(sil-serialize-all)
    let _base: _UnmanagedString
    @usableFromInline // FIXME(sil-serialize-all)
    var _offset: Int

    @inlinable // FIXME(sil-serialize-all)
    init(_ base: _UnmanagedString) {
      self._base = base
      self._offset = 0
    }

    @inlinable // FIXME(sil-serialize-all)
    mutating func next() -> Unicode.Scalar? {
      if _slowPath(_offset == _base.count) { return nil }
      let u0 = _base[_offset]
      if _fastPath(CodeUnit.bitWidth == 8 || UTF16._isScalar(u0)) {
        _offset += 1
        return Unicode.Scalar(u0)
      }
      if UTF16.isLeadSurrogate(u0) && _offset + 1 < _base.count {
        let u1 = _base[_offset + 1]
        if UTF16.isTrailSurrogate(u1) {
          _offset += 2
          return UTF16._decodeSurrogates(u0, u1)
        }
      }
      _offset += 1
      return Unicode.Scalar._replacementCharacter
    }
  }

  @inlinable
  func makeUnicodeScalarIterator() -> UnicodeScalarIterator {
    return UnicodeScalarIterator(self)
  }
}
