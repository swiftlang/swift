//===--- DropWhile.swift - Lazy views for drop(while:) --------*- swift -*-===//
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

/// A sequence whose elements consist of the elements that follow the initial
/// consecutive elements of some base sequence that satisfy a given predicate.
@_fixed_layout // lazy-performance
public struct LazyDropWhileSequence<Base: Sequence> {
  public typealias Element = Base.Element
  
  /// Create an instance with elements `transform(x)` for each element
  /// `x` of base.
  @inlinable // lazy-performance
  internal init(_base: Base, predicate: @escaping (Element) -> Bool) {
    self._base = _base
    self._predicate = predicate
  }

  @usableFromInline // lazy-performance
  internal var _base: Base
  @usableFromInline // lazy-performance
  internal let _predicate: (Element) -> Bool
}

extension LazyDropWhileSequence {
  /// An iterator over the elements traversed by a base iterator that follow the
  /// initial consecutive elements that satisfy a given predicate.
  ///
  /// This is the associated iterator for the `LazyDropWhileSequence`,
  /// `LazyDropWhileCollection`, and `LazyDropWhileBidirectionalCollection`
  /// types.
  @_fixed_layout // lazy-performance
  public struct Iterator {
    public typealias Element = Base.Element
    
    @inlinable // lazy-performance
    internal init(_base: Base.Iterator, predicate: @escaping (Element) -> Bool) {
      self._base = _base
      self._predicate = predicate
    }

    @usableFromInline // lazy-performance
    internal var _predicateHasFailed = false
    @usableFromInline // lazy-performance
    internal var _base: Base.Iterator
    @usableFromInline // lazy-performance
    internal let _predicate: (Element) -> Bool
  }
}

extension LazyDropWhileSequence.Iterator: IteratorProtocol {
  @inlinable // lazy-performance
  public mutating func next() -> Element? {
    // Once the predicate has failed for the first time, the base iterator
    // can be used for the rest of the elements.
    if _predicateHasFailed {
      return _base.next()
    }

    // Retrieve and discard elements from the base iterator until one fails
    // the predicate.
    while let nextElement = _base.next() {
      if !_predicate(nextElement) {
        _predicateHasFailed = true
        return nextElement
      }
    }
    return nil
  }  
}

extension LazyDropWhileSequence: Sequence {
  public typealias SubSequence = AnySequence<Element> // >:(

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @inlinable // lazy-performance
  public __consuming func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator(), predicate: _predicate)
  }
}

extension LazyDropWhileSequence: LazySequenceProtocol {
  public typealias Elements = LazyDropWhileSequence
}

extension LazySequenceProtocol {
  /// Returns a lazy sequence that skips any initial elements that satisfy
  /// `predicate`.
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence as
  ///   its argument and returns `true` if the element should be skipped or
  ///   `false` otherwise. Once `predicate` returns `false` it will not be
  ///   called again.
  @inlinable // lazy-performance
  public __consuming func drop(
    while predicate: @escaping (Elements.Element) -> Bool
  ) -> LazyDropWhileSequence<Self.Elements> {
    return LazyDropWhileSequence(_base: self.elements, predicate: predicate)
  }
}

/// A lazy wrapper that includes the elements of an underlying
/// collection after any initial consecutive elements that satisfy a
/// predicate.
///
/// - Note: The performance of accessing `startIndex`, `first`, or any methods
///   that depend on `startIndex` depends on how many elements satisfy the
///   predicate at the start of the collection, and may not offer the usual
///   performance given by the `Collection` protocol. Be aware, therefore,
///   that general operations on lazy collections may not have the
///   documented complexity.
@_fixed_layout // lazy-performance
public struct LazyDropWhileCollection<Base: Collection> {
  public typealias Element = Base.Element
  
  @inlinable // lazy-performance
  internal init(_base: Base, predicate: @escaping (Element) -> Bool) {
    self._base = _base
    self._predicate = predicate
  }

  @usableFromInline // lazy-performance
  internal var _base: Base
  @usableFromInline // lazy-performance
  internal let _predicate: (Element) -> Bool
}

extension LazyDropWhileCollection: Sequence {
  public typealias Iterator = LazyDropWhileSequence<Base>.Iterator
  
  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @inlinable // lazy-performance
  public __consuming func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator(), predicate: _predicate)
  }
}

extension LazyDropWhileCollection {
  public typealias SubSequence = Slice<LazyDropWhileCollection<Base>>

  /// A position in a `LazyDropWhileCollection` or
  /// `LazyDropWhileBidirectionalCollection` instance.
  @_fixed_layout // lazy-performance
  public struct Index {
    /// The position corresponding to `self` in the underlying collection.
    public let base: Base.Index

    @inlinable // lazy-performance
    internal init(_base: Base.Index) {
      self.base = _base
    }
  }
}

extension LazyDropWhileCollection.Index: Equatable, Comparable {
  @inlinable // lazy-performance
  public static func == (
    lhs: LazyDropWhileCollection<Base>.Index,
    rhs: LazyDropWhileCollection<Base>.Index
  ) -> Bool {
    return lhs.base == rhs.base
  }

  @inlinable // lazy-performance
  public static func < (
    lhs: LazyDropWhileCollection<Base>.Index,
    rhs: LazyDropWhileCollection<Base>.Index
  ) -> Bool {
    return lhs.base < rhs.base
  }
}

extension LazyDropWhileCollection.Index: Hashable where Base.Index: Hashable {
  /// The hash value.
  @inlinable
  public var hashValue: Int {
    return base.hashValue
  }

  /// Hashes the essential components of this value by feeding them into the
  /// given hasher.
  ///
  /// - Parameter hasher: The hasher to use when combining the components
  ///   of this instance.
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(base)
  }
}

extension LazyDropWhileCollection: Collection {
  @inlinable // lazy-performance
  public var startIndex: Index {
    var index = _base.startIndex
    while index != _base.endIndex && _predicate(_base[index]) {
      _base.formIndex(after: &index)
    }
    return Index(_base: index)
  }

  @inlinable // lazy-performance
  public var endIndex: Index {
    return Index(_base: _base.endIndex)
  }

  @inlinable // lazy-performance
  public func index(after i: Index) -> Index {
    _precondition(i.base < _base.endIndex, "Can't advance past endIndex")
    return Index(_base: _base.index(after: i.base))
  }


  @inlinable // lazy-performance
  public subscript(position: Index) -> Element {
    _read {
      yield _base[position.base]
    }
  }
}

extension LazyDropWhileCollection: LazyCollectionProtocol { }

extension LazyDropWhileCollection: BidirectionalCollection 
where Base: BidirectionalCollection {
  @inlinable // lazy-performance
  public func index(before i: Index) -> Index {
    _precondition(i > startIndex, "Can't move before startIndex")
    return Index(_base: _base.index(before: i.base))
  }
}

extension LazyCollectionProtocol {
  /// Returns a lazy collection that skips any initial elements that satisfy
  /// `predicate`.
  ///
  /// - Parameter predicate: A closure that takes an element of the collection
  ///   as its argument and returns `true` if the element should be skipped or
  ///   `false` otherwise. Once `predicate` returns `false` it will not be
  ///   called again.
  @inlinable // lazy-performance
  public __consuming func drop(
    while predicate: @escaping (Elements.Element) -> Bool
  ) -> LazyDropWhileCollection<Self.Elements> {
    return LazyDropWhileCollection(
      _base: self.elements, predicate: predicate)
  }
}

