//===--- ValidUTF8Buffer.swift - Bounded Collection of Valid UTF-8 --------===//
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
//
//  Stores valid UTF8 inside an unsigned integer.
//
//  Actually this basic type could be used to store any UInt8s that cannot be
//  0xFF
//
//===----------------------------------------------------------------------===//
@_fixed_layout
public struct _ValidUTF8Buffer {
  public typealias Element = Unicode.UTF8.CodeUnit

  @usableFromInline
  internal var _biasedBits: UInt32

  @inlinable
  internal init(_biasedBits: UInt32) {
    self._biasedBits = _biasedBits
  }

  @inlinable
  internal init(_containing e: Element) {
    _sanityCheck(
      e != 192 && e != 193 && !(245...255).contains(e), "invalid UTF8 byte")
    _biasedBits = UInt32(truncatingIfNeeded: e &+ 1)
  }
}

extension _ValidUTF8Buffer : Sequence {
  public typealias SubSequence = Slice<_ValidUTF8Buffer>

  @_fixed_layout
  public struct Iterator : IteratorProtocol, Sequence {
    @inlinable
    public init(_ x: _ValidUTF8Buffer) { _biasedBits = x._biasedBits }

    @inlinable
    public mutating func next() -> Element? {
      if _biasedBits == 0 { return nil }
      defer { _biasedBits >>= 8 }
      return Element(truncatingIfNeeded: _biasedBits) &- 1
    }
    @usableFromInline
    internal var _biasedBits: UInt32
  }

  @inlinable
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _ValidUTF8Buffer : Collection {
  @_fixed_layout
  public struct Index : Comparable {
    @usableFromInline
    internal var _biasedBits: UInt32

    @inlinable
    internal init(_biasedBits: UInt32) { self._biasedBits = _biasedBits }

    @inlinable
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs._biasedBits == rhs._biasedBits
    }
    @inlinable
    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs._biasedBits > rhs._biasedBits
    }
  }

  @inlinable
  public var startIndex : Index {
    return Index(_biasedBits: _biasedBits)
  }

  @inlinable
  public var endIndex : Index {
    return Index(_biasedBits: 0)
  }

  @inlinable
  public var count : Int {
    return UInt32.bitWidth &>> 3 &- _biasedBits.leadingZeroBitCount &>> 3
  }

  @inlinable
  public var isEmpty : Bool {
    return _biasedBits == 0
  }

  @inlinable
  public func index(after i: Index) -> Index {
    _debugPrecondition(i._biasedBits != 0)
    return Index(_biasedBits: i._biasedBits >> 8)
  }

  @inlinable
  public subscript(i: Index) -> Element {
    return Element(truncatingIfNeeded: i._biasedBits) &- 1
  }
}

extension _ValidUTF8Buffer : BidirectionalCollection {
  @inlinable
  public func index(before i: Index) -> Index {
    let offset = _ValidUTF8Buffer(_biasedBits: i._biasedBits).count
    _debugPrecondition(offset != 0)
    return Index(_biasedBits: _biasedBits &>> (offset &<< 3 - 8))
  }
}

extension _ValidUTF8Buffer : RandomAccessCollection {
  public typealias Indices = DefaultIndices<_ValidUTF8Buffer>

  @inlinable
  @inline(__always)
  public func distance(from i: Index, to j: Index) -> Int {
    _debugPrecondition(_isValid(i))
    _debugPrecondition(_isValid(j))
    return (
      i._biasedBits.leadingZeroBitCount - j._biasedBits.leadingZeroBitCount
    ) &>> 3
  }

  @inlinable
  @inline(__always)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    let startOffset = distance(from: startIndex, to: i)
    let newOffset = startOffset + n
    _debugPrecondition(newOffset >= 0)
    _debugPrecondition(newOffset <= count)
    return Index(_biasedBits: _biasedBits._fullShiftRight(newOffset &<< 3))
  }
}

extension _ValidUTF8Buffer : RangeReplaceableCollection {
  @inlinable
  public init() {
    _biasedBits = 0
  }

  @inlinable
  public var capacity: Int {
    return _ValidUTF8Buffer.capacity
  }

  @inlinable
  public static var capacity: Int {
    return UInt32.bitWidth / Element.bitWidth
  }

  @inlinable
  @inline(__always)
  public mutating func append(_ e: Element) {
    _debugPrecondition(count + 1 <= capacity)
    _sanityCheck(
      e != 192 && e != 193 && !(245...255).contains(e), "invalid UTF8 byte")
    _biasedBits |= UInt32(e &+ 1) &<< (count &<< 3)
  }

  @inlinable
  @inline(__always)
  @discardableResult
  public mutating func removeFirst() -> Element {
    _debugPrecondition(!isEmpty)
    let result = Element(truncatingIfNeeded: _biasedBits) &- 1
    _biasedBits = _biasedBits._fullShiftRight(8)
    return result
  }

  @inlinable
  internal func _isValid(_ i: Index) -> Bool {
    return i == endIndex || indices.contains(i)
  }

  @inlinable
  @inline(__always)
  public mutating func replaceSubrange<C: Collection>(
    _ target: Range<Index>, with replacement: C
  ) where C.Element == Element {
    _debugPrecondition(_isValid(target.lowerBound))
    _debugPrecondition(_isValid(target.upperBound))
    var r = _ValidUTF8Buffer()
    for x in self[..<target.lowerBound] { r.append(x) }
    for x in replacement                { r.append(x) }
    for x in self[target.upperBound...] { r.append(x) }
    self = r
  }
}

extension _ValidUTF8Buffer {
  @inlinable
  @inline(__always)
  public mutating func append(contentsOf other: _ValidUTF8Buffer) {
    _debugPrecondition(count + other.count <= capacity)
    _biasedBits |= UInt32(
      truncatingIfNeeded: other._biasedBits) &<< (count &<< 3)
  }
}

extension _ValidUTF8Buffer {
  @inlinable
  public static var encodedReplacementCharacter : _ValidUTF8Buffer {
    return _ValidUTF8Buffer(_biasedBits: 0xBD_BF_EF &+ 0x01_01_01)
  }

  @inlinable
  internal var _bytes: (bytes: UInt64, count: Int) {
    let count = self.count
    let mask: UInt64 = 1 &<< (UInt64(truncatingIfNeeded: count) &<< 3) &- 1
    let unbiased = UInt64(truncatingIfNeeded: _biasedBits) &- 0x0101010101010101
    return (unbiased & mask, count)
  }
}
