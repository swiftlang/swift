//===--- ChunkedCollection.swift ----------------------------------------------------===//
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

/// A collection that presents the elements of its base collection
/// in `SubSequence` chunks of any given size.
///
/// A ChunkedCollection is a lazy view on the base Collection, but it does not implicitly confer
/// laziness on algorithms applied to its result.  In other words, for ordinary collections `c`:
///
/// * `c.chunks(of: 3)` does not create new storage
/// * `c.chunks(of: 3).map(f)` maps eagerly and returns a new array
/// * `c.lazy.chunks(of: 3).map(f)` maps lazily and returns a `LazyMapCollection`
@_fixed_layout
public struct ChunkedCollection<Base: Collection> {
    @usableFromInline
    internal let _base: Base
    @usableFromInline
    internal let _size: Int
    
    ///  Creates a view instance that presents the elements of `base`
    ///  in `SubSequence` chunks of the given size.
    ///
    /// - Complexity: O(1)
    @inlinable
    internal init(_base: Base, _size: Int) {
      self._base = _base
      self._size = _size
    }
}

extension ChunkedCollection: Collection {
  public struct Index {
    @usableFromInline
    let _base: Base.Index
        
    @usableFromInline
    init(_base: Base.Index) {
      self._base = _base
    }
  }
    
  public typealias Element = Base.SubSequence
  public var startIndex: Index { return Index(_base: _base.startIndex) }
  public var endIndex: Index { return Index(_base: _base.endIndex) }
    
  public subscript(i: Index) -> Element {
    let range = i..<index(after: i)
    return _base[range.lowerBound._base..<range.upperBound._base]
  }
    
  @inlinable
  public func index(after i: Index) -> Index {
    return Index(_base: _base.index(i._base, offsetBy: _size, limitedBy: _base.endIndex) ?? _base.endIndex)
  }
}

extension ChunkedCollection.Index: Comparable {
  public static func < (lhs: ChunkedCollection<Base>.Index, rhs: ChunkedCollection<Base>.Index) -> Bool {
    return lhs._base < rhs._base
  }
}

extension ChunkedCollection:
    BidirectionalCollection, RandomAccessCollection
where Base: RandomAccessCollection {
  @inlinable
  public func index(before i: Index) -> Index {
    if i._base == _base.endIndex {
      let remainder = _base.count%_size
      if remainder != 0 {
        return Index(_base: _base.index(i._base, offsetBy: -remainder))
      }
    }
    return Index(_base: _base.index(i._base, offsetBy: -_size))
  }
    
  @inlinable
  public func distance(from start: Index, to end: Index) -> Int {
    let distance = _base.distance(from: start._base, to: end._base)/_size
    return _base.count.isMultiple(of: _size) ? distance : distance + 1
  }
    
  @inlinable
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    guard n != 0 else { return i }
    return Index(_base: _base.index(i._base, offsetBy: n * _size))
  }
    
  @inlinable
  public var count: Int {
    let count = _base.count/_size
    return _base.count.isMultiple(of: _size) ? count : count + 1
  }
}

extension Collection {
  /// Returns a `ChunkedCollection<Self>` view presenting the elements
  ///    in chunks with count of the given size parameter.
  ///
  /// - Parameter size: The size of the chunks. If the size parameter
  ///   is evenly divided by the count of the base `Collection` all the
  ///   chunks will have the count equals to size.
  ///   Otherwise, the last chunk will contain the remaining elements.
  ///
  ///     let c = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  ///     print(c.chunks(of: 5).map(Array.init))
  ///     // [[1, 2, 3, 4, 5], [6, 7, 8, 9, 10]]
  ///
  ///     print(c.chunks(of: 3).map(Array.init))
  ///     // [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]]
  ///
  /// - Complexity: O(1)
  @inlinable
  public __consuming func chunks(of size: Int) -> ChunkedCollection<Self> {
    _precondition(size > 0, "Split size should be greater than 0.")
    return ChunkedCollection(_base: self, _size: size)
  }
}
