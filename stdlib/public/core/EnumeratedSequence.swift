//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// An enumeration of the elements of a sequence or collection.
///
/// `EnumeratedSequence` is a sequence of pairs (*n*, *x*), where *n*s are
/// consecutive `Int` values starting at zero, and *x*s are the elements of a
/// base sequence.
///
/// To create an instance of `EnumeratedSequence`, call `enumerated()` on a
/// sequence or collection. The following example enumerates the elements of
/// an array.
///
///     var s = ["foo", "bar"].enumerated()
///     for (n, x) in s {
///         print("\(n): \(x)")
///     }
///     // Prints "0: foo"
///     // Prints "1: bar"
@frozen
public struct EnumeratedSequence<Base: Sequence> {
  @usableFromInline
  internal var _base: Base

  /// Construct from a `Base` sequence.
  @inlinable
  internal init(_base: Base) {
    self._base = _base
  }
}

extension EnumeratedSequence: Sendable where Base: Sendable {}

extension EnumeratedSequence {
  /// The iterator for `EnumeratedSequence`.
  ///
  /// An instance of this iterator wraps a base iterator and yields
  /// successive `Int` values, starting at zero, along with the elements of the
  /// underlying base iterator. The following example enumerates the elements of
  /// an array:
  ///
  ///     var iterator = ["foo", "bar"].enumerated().makeIterator()
  ///     iterator.next() // (0, "foo")
  ///     iterator.next() // (1, "bar")
  ///     iterator.next() // nil
  ///
  /// To create an instance, call
  /// `enumerated().makeIterator()` on a sequence or collection.
  @frozen
  public struct Iterator {
    @usableFromInline
    internal var _base: Base.Iterator
    @usableFromInline
    internal var _count: Int

    /// Construct from a `Base` iterator.
    @inlinable
    internal init(_base: Base.Iterator) {
      self._base = _base
      self._count = 0
    }
  }
}

extension EnumeratedSequence.Iterator: Sendable where Base.Iterator: Sendable {}

extension EnumeratedSequence.Iterator: IteratorProtocol, Sequence {
  /// The type of element returned by `next()`.
  public typealias Element = (offset: Int, element: Base.Element)

  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  @inlinable
  public mutating func next() -> Element? {
    guard let b = _base.next() else { return nil }
    let result = (offset: _count, element: b)
    _count += 1 
    return result
  }
}

extension EnumeratedSequence: Sequence {
  /// Returns an iterator over the elements of this sequence.
  @inlinable
  public __consuming func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator())
  }
}

@available(SwiftStdlib 6.2, *)
extension EnumeratedSequence: Collection where Base: Collection {
  @available(SwiftStdlib 6.2, *)
  @frozen
  public struct Index {
    /// The position in the underlying collection.
    public let base: Base.Index

    /// The offset corresponding to this index when `base` is not the end index,
    /// `0` otherwise.
    @usableFromInline
    let _offset: Int

    @available(SwiftStdlib 6.2, *)
    @_alwaysEmitIntoClient
    init(base: Base.Index, offset: Int) {
      self.base = base
      self._offset = offset
    }
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public var startIndex: Index {
    Index(base: _base.startIndex, offset: 0)
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public var endIndex: Index {
    Index(base: _base.endIndex, offset: 0)
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public var count: Int {
    _base.count
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public var isEmpty: Bool {
    _base.isEmpty
  }

  /// Returns the offset corresponding to `index`.
  ///
  /// - Complexity: O(*n*) if `index == endIndex` and `Base` does not conform to
  ///   `RandomAccessCollection`, O(1) otherwise.
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  func _offset(of index: Index) -> Int {
    index.base == _base.endIndex ? _base.count : index._offset
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public func distance(from start: Index, to end: Index) -> Int {
    if start.base == _base.endIndex || end.base == _base.endIndex {
      return _base.distance(from: start.base, to: end.base)
    } else {
      return end._offset - start._offset
    }
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public func index(after index: Index) -> Index {
    Index(base: _base.index(after: index.base), offset: index._offset + 1)
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public func index(_ i: Index, offsetBy distance: Int) -> Index {
    let index = _base.index(i.base, offsetBy: distance)
    let offset = distance >= 0 ? i._offset : _offset(of: i)
    return Index(base: index, offset: offset + distance)
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public func index(
    _ i: Index,
    offsetBy distance: Int,
    limitedBy limit: Index
  ) -> Index? {
    guard let index = _base.index(
      i.base,
      offsetBy: distance,
      limitedBy: limit.base
    ) else {
      return nil
    }

    let offset = distance >= 0 ? i._offset : _offset(of: i)
    return Index(base: index, offset: offset + distance)
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public subscript(_ position: Index) -> Element {
    _precondition(
      _base.startIndex <= position.base && position.base < _base.endIndex,
      "Index out of bounds"
    )

    return (position._offset, _base[position.base])
  }
}

@available(SwiftStdlib 6.2, *)
extension EnumeratedSequence.Index: Comparable {
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    lhs.base == rhs.base
  }

  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public static func <(lhs: Self, rhs: Self) -> Bool {
    lhs.base < rhs.base
  }
}

@available(SwiftStdlib 6.2, *)
extension EnumeratedSequence: BidirectionalCollection where Base: BidirectionalCollection & RandomAccessCollection {
  @available(SwiftStdlib 6.2, *)
  @_alwaysEmitIntoClient
  public func index(before index: Index) -> Index {
    Index(base: _base.index(before: index.base), offset: _offset(of: index) - 1)
  }
}

@available(SwiftStdlib 6.2, *)
extension EnumeratedSequence: RandomAccessCollection where Base: RandomAccessCollection {}
