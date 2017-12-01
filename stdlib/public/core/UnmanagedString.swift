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

@_fixed_layout
@_versioned
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

  @_versioned
  internal var start: UnsafePointer<CodeUnit>

  @_versioned
  internal var count: Int

  @_versioned
  init(start: UnsafePointer<CodeUnit>, count: Int) {
    _sanityCheck(CodeUnit.self == UInt8.self || CodeUnit.self == UInt16.self)
    self.start = start
    self.count = count
  }
}

extension _UnmanagedString {
  @_inlineable
  @_versioned
  internal var end: UnsafePointer<CodeUnit> {
    return start + count
  }

  @_inlineable
  @_versioned
  internal var isASCII: Bool {
    // NOTE: For now, single byte means ASCII. Might change in future
    return CodeUnit.bitWidth == 8
  }

  @_inlineable
  @_versioned
  internal var rawStart: UnsafeRawPointer {
    return UnsafeRawPointer(start)
  }

  @_inlineable
  @_versioned
  internal var rawEnd: UnsafeRawPointer {
    return UnsafeRawPointer(end)
  }

  @_inlineable
  @_versioned
  internal var buffer: UnsafeBufferPointer<CodeUnit> {
    return .init(start: start, count: count)
  }

  @_inlineable
  @_versioned
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
  internal typealias Indices = CountableRange<Index>
  internal typealias SubSequence = _UnmanagedString

  @_inlineable
  @_versioned
  internal
  var startIndex: Index { return start }

  @_inlineable
  @_versioned
  internal
  var endIndex: Index { return end }

  @_inlineable
  @_versioned
  internal subscript(position: Index) -> UTF16.CodeUnit {
    @inline(__always)
    get {
      _sanityCheck(position >= start && position < end)
      return UTF16.CodeUnit(position.pointee)
    }
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal subscript(_ bounds: Range<Index>) -> SubSequence {
    _sanityCheck(bounds.lowerBound >= start && bounds.upperBound <= end)
    return _UnmanagedString(start: bounds.lowerBound, count: bounds.count)
  }
}

extension _UnmanagedString {
  @_inlineable
  @_versioned
  internal subscript(position: Int) -> UTF16.CodeUnit {
    @inline(__always)
    get {
      _sanityCheck(position >= 0 && position < count)
      return UTF16.CodeUnit(start[position])
    }
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal subscript(_ bounds: Range<Int>) -> _UnmanagedString {
    _sanityCheck(bounds.lowerBound >= 0 && bounds.upperBound <= count)
    return _UnmanagedString(
      start: start + bounds.lowerBound,
      count: bounds.count)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func offset(of iterator: Iterator) -> Int {
    return iterator._position - start
  }
}

extension _UnmanagedString {
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func makeIterator(startingAt position: Index) -> Iterator {
    return Iterator(_elements: self, _position: position)
  }

  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func makeIterator(startingAt offset: Int) -> Iterator {
    return Iterator(_elements: self, _position: startIndex + offset)
  }
}

extension _UnmanagedString {
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
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
      var t = target.baseAddress!
      for byte in self {
        t.pointee = TargetCodeUnit(truncatingIfNeeded: byte)
        t += 1
      }
    } else {
      _sanityCheck(CodeUnit.bitWidth == 16 && TargetCodeUnit.bitWidth == 8)
      var t = target.baseAddress!
      for unit in self {
        _sanityCheck(unit & ~0x7F == 0) // ASCII only
        t.pointee = TargetCodeUnit(truncatingIfNeeded: unit)
        t += 1
      }
    }
  }
}
