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
public struct _ValidUTF8Buffer<
  Storage: UnsignedInteger & FixedWidthInteger
> {
  public typealias Element = Unicode.UTF8.CodeUnit
  internal typealias _Storage = Storage
  
  @_versioned
  internal var _biasedBits: Storage

  @_versioned
  internal init(_biasedBits: Storage) {
    self._biasedBits = _biasedBits
  }
  
  @_versioned
  internal init(_containing e: Element) {
    _sanityCheck(
      e != 192 && e != 193 && !(245...255).contains(e), "invalid UTF8 byte")
    _biasedBits = Storage(truncatingIfNeeded: e &+ 1)
  }
}

extension _ValidUTF8Buffer : Sequence {
  public typealias SubSequence = RangeReplaceableRandomAccessSlice<_ValidUTF8Buffer>
  
  public struct Iterator : IteratorProtocol, Sequence {
    public init(_ x: _ValidUTF8Buffer) { _biasedBits = x._biasedBits }
    
    public mutating func next() -> Element? {
      if _biasedBits == 0 { return nil }
      defer { _biasedBits >>= 8 }
      return Element(truncatingIfNeeded: _biasedBits) &- 1
    }
    internal var _biasedBits: Storage
  }
  
  public func makeIterator() -> Iterator {
    return Iterator(self)
  }
}

extension _ValidUTF8Buffer : Collection {  
  public typealias IndexDistance = Int
  
  public struct Index : Comparable {
    @_versioned
    internal var _biasedBits: Storage
    
    @_versioned
    internal init(_biasedBits: Storage) { self._biasedBits = _biasedBits }
    
    public static func == (lhs: Index, rhs: Index) -> Bool {
      return lhs._biasedBits == rhs._biasedBits
    }
    public static func < (lhs: Index, rhs: Index) -> Bool {
      return lhs._biasedBits > rhs._biasedBits
    }
  }

  public var startIndex : Index {
    return Index(_biasedBits: _biasedBits)
  }
  
  public var endIndex : Index {
    return Index(_biasedBits: 0)
  }

  public var count : IndexDistance {
    return Storage.bitWidth &>> 3 &- _biasedBits.leadingZeroBitCount &>> 3
  }
  
  public func index(after i: Index) -> Index {
    _debugPrecondition(i._biasedBits != 0)
    return Index(_biasedBits: i._biasedBits >> 8)
  }

  public subscript(i: Index) -> Element {
    return Element(truncatingIfNeeded: i._biasedBits) &- 1
  }
}

extension _ValidUTF8Buffer : BidirectionalCollection {
  public func index(before i: Index) -> Index {
    let offset = _ValidUTF8Buffer(_biasedBits: i._biasedBits).count
    _debugPrecondition(offset != 0)
    return Index(_biasedBits: _biasedBits &>> (offset &<< 3 - 8))
  }
}

extension _ValidUTF8Buffer : RandomAccessCollection {
  public typealias Indices = DefaultRandomAccessIndices<_ValidUTF8Buffer>

  @inline(__always)
  public func distance(from i: Index, to j: Index) -> IndexDistance {
    _debugPrecondition(_isValid(i))
    _debugPrecondition(_isValid(j))
    return (
      i._biasedBits.leadingZeroBitCount - j._biasedBits.leadingZeroBitCount
    ) &>> 3
  }
  
  @inline(__always)
  public func index(_ i: Index, offsetBy n: IndexDistance) -> Index {
    let startOffset = distance(from: startIndex, to: i)
    let newOffset = startOffset + n
    _debugPrecondition(newOffset >= 0)
    _debugPrecondition(newOffset <= count)
    return Index(_biasedBits: _biasedBits._fullShiftRight(newOffset &<< 3))
  }
}

extension _ValidUTF8Buffer : RangeReplaceableCollection {
  public init() {
    _biasedBits = 0
  }

  public var capacity: IndexDistance {
    return _ValidUTF8Buffer.capacity
  }

  public static var capacity: IndexDistance {
    return Storage.bitWidth / Element.bitWidth
  }

  @inline(__always)
  public mutating func append(_ e: Element) {
    _debugPrecondition(count + 1 <= capacity)
    _sanityCheck(
      e != 192 && e != 193 && !(245...255).contains(e), "invalid UTF8 byte")
    _biasedBits |= Storage(e &+ 1) &<< (count &<< 3)
  }

  @inline(__always)
  public mutating func removeFirst() {
    _debugPrecondition(!isEmpty)
    _biasedBits = _biasedBits._fullShiftRight(8)
  }

  @_versioned
  internal func _isValid(_ i: Index) -> Bool {
    return i == endIndex || indices.contains(i)
  }
  
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

  @inline(__always)
  public mutating func append<T>(contentsOf other: _ValidUTF8Buffer<T>) {
    _debugPrecondition(count + other.count <= capacity)
    _biasedBits |= Storage(
      truncatingIfNeeded: other._biasedBits) &<< (count &<< 3)
  }
}

extension _ValidUTF8Buffer {
  public static var encodedReplacementCharacter : _ValidUTF8Buffer {
    return _ValidUTF8Buffer(_biasedBits: 0xBD_BF_EF &+ 0x01_01_01)
  }
}

/*
let test = _ValidUTF8Buffer<UInt64>(0..<8)
print(Array(test))
print(test.startIndex)
for (ni, i) in test.indices.enumerated() {
  for (nj, j) in test.indices.enumerated() {
    assert(test.distance(from: i, to: j) == nj - ni)
    assert(test.index(i, offsetBy: nj - ni) == j)
  }
}
*/
