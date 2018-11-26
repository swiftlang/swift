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
public struct _ValidUTF8Buffer<Storage: UnsignedInteger & FixedWidthInteger> {
  public typealias Element = Unicode.UTF8.CodeUnit
  internal typealias _Storage = Storage
  
  @usableFromInline
  internal var _biasedBits: Storage

  @inlinable // FIXME(sil-serialize-all)
  internal init(_biasedBits: Storage) {
    self._biasedBits = _biasedBits
  }
  
  @inlinable // FIXME(sil-serialize-all)
  internal init(_containing e: Element) {
    _sanityCheck(
      e != 192 && e != 193 && !(245...255).contains(e), "invalid UTF8 byte")
    _biasedBits = Storage(truncatingIfNeeded: e &+ 1)
  }
}

extension _ValidUTF8Buffer : Sequence {
  public typealias SubSequence = Slice<_ValidUTF8Buffer>
  

  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Iterator : IteratorProtocol, Sequence {
    @inlinable // FIXME(sil-serialize-all)
    public init(_ x: _ValidUTF8Buffer) { _biasedBits = x._biasedBits }
    
    @inlinable // FIXME(sil-serialize-all)
    public mutating func next() -> Element? {
      if _biasedBits == 0 { return nil }
      defer { _biasedBits >>= 8 }
      return Element(truncatingIfNeeded: _biasedBits) &- 1
    }
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _biasedBits: Storage
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _ValidUTF8Buffer : Collection {  
  
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Index : Comparable {
    @usableFromInline
    internal var _biasedBits: Storage
    
    @inlinable // FIXME(sil-serialize-all)
    internal init(_biasedBits: Storage) { self._biasedBits = _biasedBits }
    
    @inlinable // FIXME(sil-serialize-all)
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs._biasedBits == rhs._biasedBits
    }
    @inlinable // FIXME(sil-serialize-all)
    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs._biasedBits > rhs._biasedBits
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  public var startIndex : Index {
    return Index(_biasedBits: _biasedBits)
  }
  
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex : Index {
    return Index(_biasedBits: 0)
  }

  @inlinable // FIXME(sil-serialize-all)
  public var count : Int {
    return Storage.bitWidth &>> 3 &- _biasedBits.leadingZeroBitCount &>> 3
  }

  @inlinable // FIXME(sil-serialize-all)
  public var isEmpty : Bool {
    return _biasedBits == 0
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(after i: Index) -> Index {
    _debugPrecondition(i._biasedBits != 0)
    return Index(_biasedBits: i._biasedBits >> 8)
  }

  @inlinable // FIXME(sil-serialize-all)
  public subscript(i: Index) -> Element {
    return Element(truncatingIfNeeded: i._biasedBits) &- 1
  }
}

extension _ValidUTF8Buffer : BidirectionalCollection {
  @inlinable // FIXME(sil-serialize-all)
  public func index(before i: Index) -> Index {
    let offset = _ValidUTF8Buffer(_biasedBits: i._biasedBits).count
    _debugPrecondition(offset != 0)
    return Index(_biasedBits: _biasedBits &>> (offset &<< 3 - 8))
  }
}

extension _ValidUTF8Buffer : RandomAccessCollection {
  public typealias Indices = DefaultIndices<_ValidUTF8Buffer>

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public func distance(from i: Index, to j: Index) -> Int {
    _debugPrecondition(_isValid(i))
    _debugPrecondition(_isValid(j))
    return (
      i._biasedBits.leadingZeroBitCount - j._biasedBits.leadingZeroBitCount
    ) &>> 3
  }
  
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
  public init() {
    _biasedBits = 0
  }

  @inlinable // FIXME(sil-serialize-all)
  public var capacity: Int {
    return _ValidUTF8Buffer.capacity
  }

  @inlinable // FIXME(sil-serialize-all)
  public static var capacity: Int {
    return Storage.bitWidth / Element.bitWidth
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public mutating func append(_ e: Element) {
    _debugPrecondition(count + 1 <= capacity)
    _sanityCheck(
      e != 192 && e != 193 && !(245...255).contains(e), "invalid UTF8 byte")
    _biasedBits |= Storage(e &+ 1) &<< (count &<< 3)
  }

  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  @discardableResult
  public mutating func removeFirst() -> Element {
    _debugPrecondition(!isEmpty)
    let result = Element(truncatingIfNeeded: _biasedBits) &- 1
    _biasedBits = _biasedBits._fullShiftRight(8)
    return result
  }

  @inlinable // FIXME(sil-serialize-all)
  internal func _isValid(_ i: Index) -> Bool {
    return i == endIndex || indices.contains(i)
  }
  
  @inlinable // FIXME(sil-serialize-all)
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
  @inlinable // FIXME(sil-serialize-all)
  @inline(__always)
  public mutating func append<T>(contentsOf other: _ValidUTF8Buffer<T>) {
    _debugPrecondition(count + other.count <= capacity)
    _biasedBits |= Storage(
      truncatingIfNeeded: other._biasedBits) &<< (count &<< 3)
  }
}

extension _ValidUTF8Buffer {
  @inlinable // FIXME(sil-serialize-all)
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
