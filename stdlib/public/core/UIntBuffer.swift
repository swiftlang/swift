//===--- UIntBuffer.swift - Bounded Collection of Unsigned Integer --------===//
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
//  Stores a smaller unsigned integer type inside a larger one, with a limit of
//  255 elements.
//
//===----------------------------------------------------------------------===//
@frozen
public struct _UIntBuffer<Element: UnsignedInteger & FixedWidthInteger> {
  public typealias Storage = UInt32
  public var _storage: Storage
  public var _bitCount: UInt8

  @inlinable
  @inline(__always)
  public init(_storage: Storage, _bitCount: UInt8) {
    self._storage = _storage
    self._bitCount = _bitCount
  }
  
  @inlinable
  @inline(__always)
  public init(containing e: Element) {
    _storage = Storage(truncatingIfNeeded: e)
    _bitCount = UInt8(truncatingIfNeeded: Element.bitWidth)
  }
}

extension _UIntBuffer: Sendable where Element: Sendable {}

extension _UIntBuffer: Sequence {
  public typealias SubSequence = Slice<_UIntBuffer>

  @frozen
  public struct Iterator: IteratorProtocol, Sequence {
    public var _impl: _UIntBuffer

    @inlinable
    @inline(__always)
    public init(_ x: _UIntBuffer) { _impl = x }
    
    @inlinable
    @inline(__always)
    public mutating func next() -> Element? {
      if _impl._bitCount == 0 { return nil }
      defer {
        _impl._storage = _impl._storage &>> Element.bitWidth
        _impl._bitCount = _impl._bitCount &- _impl._elementWidth
      }
      return Element(truncatingIfNeeded: _impl._storage)
    }
  }
  
  @inlinable
  @inline(__always)
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _UIntBuffer.Iterator: Sendable where Element: Sendable {}

extension _UIntBuffer: Collection {
  @frozen
  public struct Index: Comparable {
    @usableFromInline
    internal var bitOffset: UInt8
    
    @inlinable
    internal init(bitOffset: UInt8) { self.bitOffset = bitOffset }
    
    @inlinable
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs.bitOffset == rhs.bitOffset
    }
    @inlinable
    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs.bitOffset < rhs.bitOffset
    }
  }

  @inlinable
  public var startIndex: Index {
    @inline(__always)
    get { return Index(bitOffset: 0) }
  }
  
  @inlinable
  public var endIndex: Index {
    @inline(__always)
    get { return Index(bitOffset: _bitCount) }
  }
  
  @inlinable
  @inline(__always)
  public func index(after i: Index) -> Index {
    return Index(bitOffset: i.bitOffset &+ _elementWidth)
  }

  @inlinable
  internal var _elementWidth: UInt8 {
    return UInt8(truncatingIfNeeded: Element.bitWidth)
  }
  
  @inlinable
  public subscript(i: Index) -> Element {
    @inline(__always)
    get {
      return Element(truncatingIfNeeded: _storage &>> i.bitOffset)
    }
  }
}

extension _UIntBuffer: BidirectionalCollection {
  @inlinable
  @inline(__always)
  public func index(before i: Index) -> Index {
    return Index(bitOffset: i.bitOffset &- _elementWidth)
  }
}

extension _UIntBuffer: RandomAccessCollection {
  public typealias Indices = DefaultIndices<_UIntBuffer>
  
  @inlinable
  @inline(__always)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    let x = Int(i.bitOffset) &+ n &* Element.bitWidth
    return Index(bitOffset: UInt8(truncatingIfNeeded: x))
  }

  @inlinable
  @inline(__always)
  public func distance(from i: Index, to j: Index) -> Int {
    return (Int(j.bitOffset) &- Int(i.bitOffset)) / Element.bitWidth
  }
}

extension FixedWidthInteger {
  @inline(__always)
  @inlinable
  internal func _fullShiftLeft<N: FixedWidthInteger>(_ n: N) -> Self {
    return (self &<< ((n &+ 1) &>> 1)) &<< (n &>> 1)
  }
  @inline(__always)
  @inlinable
  internal func _fullShiftRight<N: FixedWidthInteger>(_ n: N) -> Self {
    return (self &>> ((n &+ 1) &>> 1)) &>> (n &>> 1)
  }
  @inline(__always)
  @inlinable
  internal static func _lowBits<N: FixedWidthInteger>(_ n: N) -> Self {
    return ~((~0 as Self)._fullShiftLeft(n))
  }
}

extension Range {
  @inline(__always)
  @inlinable
  internal func _contains_(_ other: Range) -> Bool {
    return other.clamped(to: self) == other
  }
}

extension _UIntBuffer: RangeReplaceableCollection {
  @inlinable
  @inline(__always)
  public init() {
    _storage = 0
    _bitCount = 0
  }

  @inlinable
  public var capacity: Int {
    return Storage.bitWidth / Element.bitWidth
  }

  @inlinable
  @inline(__always)
  public mutating func append(_ newElement: Element) {
    _debugPrecondition(count + 1 <= capacity)
    _storage &= ~(Storage(Element.max) &<< _bitCount)
    _storage |= Storage(newElement) &<< _bitCount
    _bitCount = _bitCount &+ _elementWidth
  }

  @inlinable
  @inline(__always)
  @discardableResult
  public mutating func removeFirst() -> Element {
    _debugPrecondition(!isEmpty)
    let result = Element(truncatingIfNeeded: _storage)
    _bitCount = _bitCount &- _elementWidth
    _storage = _storage._fullShiftRight(_elementWidth)
    return result
  }
  
  @inlinable
  @inline(__always)
  public mutating func replaceSubrange<C: Collection>(
    _ target: Range<Index>, with replacement: C
  ) where C.Element == Element {
    _debugPrecondition(
      (0..<_bitCount)._contains_(
        target.lowerBound.bitOffset..<target.upperBound.bitOffset))
    
    let replacement1 = _UIntBuffer(replacement)

    let targetCount = distance(
      from: target.lowerBound, to: target.upperBound)
    let growth = replacement1.count &- targetCount
    _debugPrecondition(count + growth <= capacity)

    let headCount = distance(from: startIndex, to: target.lowerBound)
    let tailOffset = distance(from: startIndex, to: target.upperBound)

    let w = Element.bitWidth
    let headBits = _storage & ._lowBits(headCount &* w)
    let tailBits = _storage._fullShiftRight(tailOffset &* w)

    _storage = headBits
    _storage |= replacement1._storage &<< (headCount &* w)
    _storage |= tailBits &<< ((tailOffset &+ growth) &* w)
    _bitCount = UInt8(
      truncatingIfNeeded: Int(_bitCount) &+ growth &* w)
  }
}
