//===-- PrefixWhile.swift - Lazy views for prefix(while:) -----*- swift -*-===//
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


/// A sequence whose elements consist of the initial consecutive elements of
/// some base sequence that satisfy a given predicate.
@_fixed_layout // FIXME(sil-serialize-all)
public struct LazyPrefixWhileSequence<Base: Sequence> {
  public typealias Element = Base.Element
  
  @inlinable // FIXME(sil-serialize-all)
  internal init(_base: Base, predicate: @escaping (Element) -> Bool) {
    self._base = _base
    self._predicate = predicate
  }

  @usableFromInline // FIXME(sil-serialize-all)
  internal var _base: Base
  @usableFromInline // FIXME(sil-serialize-all)
  internal let _predicate: (Element) -> Bool
}


extension LazyPrefixWhileSequence {
  /// An iterator over the initial elements traversed by a base iterator that
  /// satisfy a given predicate.
  ///
  /// This is the associated iterator for the `LazyPrefixWhileSequence`,
  /// `LazyPrefixWhileCollection`, and `LazyPrefixWhileBidirectionalCollection`
  /// types.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Iterator {
    public typealias Element = Base.Element

    @usableFromInline // FIXME(sil-serialize-all)
    internal var _predicateHasFailed = false
    @usableFromInline // FIXME(sil-serialize-all)
    internal var _base: Base.Iterator
    @usableFromInline // FIXME(sil-serialize-all)
    internal let _predicate: (Element) -> Bool

    @inlinable // FIXME(sil-serialize-all)
    internal init(_base: Base.Iterator, predicate: @escaping (Element) -> Bool) {
      self._base = _base
      self._predicate = predicate
    }
  }
}

extension LazyPrefixWhileSequence.Iterator: IteratorProtocol, Sequence {
  @inlinable // FIXME(sil-serialize-all)
  public mutating func next() -> Element? {
    // Return elements from the base iterator until one fails the predicate.
    if !_predicateHasFailed, let nextElement = _base.next() {
      if _predicate(nextElement) {
        return nextElement
      } else {
        _predicateHasFailed = true
      }
    }
    return nil
  }
}

extension LazyPrefixWhileSequence: Sequence {
  public typealias SubSequence = AnySequence<Element> // >:(
  
  @inlinable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator(), predicate: _predicate)
  }
}

extension LazyPrefixWhileSequence: LazySequenceProtocol {
  public typealias Elements = LazyPrefixWhileSequence
}

extension LazySequenceProtocol {
  /// Returns a lazy sequence of the initial consecutive elements that satisfy
  /// `predicate`.
  ///
  /// - Parameter predicate: A closure that takes an element of the sequence as
  ///   its argument and returns `true` if the element should be included or
  ///   `false` otherwise. Once `predicate` returns `false` it will not be
  ///   called again.
  @inlinable // FIXME(sil-serialize-all)
  public func prefix(
    while predicate: @escaping (Elements.Element) -> Bool
  ) -> LazyPrefixWhileSequence<Self.Elements> {
    return LazyPrefixWhileSequence(_base: self.elements, predicate: predicate)
  }
}

/// A lazy `${Collection}` wrapper that includes the initial consecutive
/// elements of an underlying collection that satisfy a predicate.
///
/// - Note: The performance of accessing `endIndex`, `last`, any methods that
///   depend on `endIndex`, or moving an index depends on how many elements
///   satisfy the predicate at the start of the collection, and may not offer
///   the usual performance given by the `Collection` protocol. Be aware,
///   therefore, that general operations on `${Self}` instances may not have
///   the documented complexity.
@_fixed_layout // FIXME(sil-serialize-all)
public struct LazyPrefixWhileCollection<Base: Collection> {
  public typealias Element = Base.Element
  public typealias SubSequence = Slice<LazyPrefixWhileCollection<Base>>
  
  @inlinable // FIXME(sil-serialize-all)
  internal init(_base: Base, predicate: @escaping (Element) -> Bool) {
    self._base = _base
    self._predicate = predicate
  }

  @usableFromInline // FIXME(sil-serialize-all)
  internal var _base: Base
  @usableFromInline // FIXME(sil-serialize-all)
  internal let _predicate: (Element) -> Bool
}

extension LazyPrefixWhileCollection: Sequence {
  public typealias Iterator = LazyPrefixWhileSequence<Base>.Iterator
  
  @inlinable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator(), predicate: _predicate)
  }
}

extension LazyPrefixWhileCollection {
  /// A position in the base collection of a `LazyPrefixWhileCollection` or the
  /// end of that collection.
  @_frozen // FIXME(sil-serialize-all)
  @usableFromInline
  internal enum _IndexRepresentation {
    case index(Base.Index)
    case pastEnd
  }
  
  /// A position in a `LazyPrefixWhileCollection` or
  /// `LazyPrefixWhileBidirectionalCollection` instance.
  @_fixed_layout // FIXME(sil-serialize-all)
  public struct Index {
    /// The position corresponding to `self` in the underlying collection.
    @usableFromInline // FIXME(sil-serialize-all)
    internal let _value: _IndexRepresentation

    /// Creates a new index wrapper for `i`.
    @inlinable // FIXME(sil-serialize-all)
    internal init(_ i: Base.Index) {
      self._value = .index(i)
    }

    /// Creates a new index that can represent the `endIndex` of a
    /// `LazyPrefixWhileCollection<Base>`. This is not the same as a wrapper
    /// around `Base.endIndex`.
    @inlinable // FIXME(sil-serialize-all)
    internal init(endOf: Base) {
      self._value = .pastEnd
    }
  }
}

extension LazyPrefixWhileCollection.Index: Comparable {
  @inlinable // FIXME(sil-serialize-all)
  public static func == (
    lhs: LazyPrefixWhileCollection<Base>.Index, 
    rhs: LazyPrefixWhileCollection<Base>.Index
  ) -> Bool {
    switch (lhs._value, rhs._value) {
    case let (.index(l), .index(r)):
      return l == r
    case (.pastEnd, .pastEnd):
      return true
    case (.pastEnd, .index), (.index, .pastEnd):
      return false
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  public static func < (
    lhs: LazyPrefixWhileCollection<Base>.Index, 
    rhs: LazyPrefixWhileCollection<Base>.Index
  ) -> Bool {
    switch (lhs._value, rhs._value) {
    case let (.index(l), .index(r)):
      return l < r
    case (.index, .pastEnd):
      return true
    case (.pastEnd, _):
      return false
    }
  }
}

extension LazyPrefixWhileCollection.Index: Hashable where Base.Index: Hashable {
  @inlinable // FIXME(sil-serialize-all)
  public var hashValue: Int {
    return _hashValue(for: self)
  }

  @inlinable // FIXME(sil-serialize-all)
  public func hash(into hasher: inout Hasher) {
    switch _value {
    case .index(let value):
      hasher.combine(value)
    case .pastEnd:
      hasher.combine(Int.max)
    }
  }
}

extension LazyPrefixWhileCollection: Collection {
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    return Index(_base.startIndex)
  }

  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    // If the first element of `_base` satisfies the predicate, there is at
    // least one element in the lazy collection: Use the explicit `.pastEnd` index.
    if let first = _base.first, _predicate(first) {
      return Index(endOf: _base)
    }

    // `_base` is either empty or `_predicate(_base.first!) == false`. In either
    // case, the lazy collection is empty, so `endIndex == startIndex`.
    return startIndex
  }

  @inlinable // FIXME(sil-serialize-all)
  public func index(after i: Index) -> Index {
    _precondition(i != endIndex, "Can't advance past endIndex")
    guard case .index(let i) = i._value else {
      _preconditionFailure("Invalid index passed to index(after:)")
    }
    let nextIndex = _base.index(after: i)
    guard nextIndex != _base.endIndex && _predicate(_base[nextIndex]) else {
      return Index(endOf: _base)
    }
    return Index(nextIndex)
  }

  @inlinable // FIXME(sil-serialize-all)
  public subscript(position: Index) -> Element {
    switch position._value {
    case .index(let i):
      return _base[i]
    case .pastEnd:
      _preconditionFailure("Index out of range")
    }
  }
}

extension LazyPrefixWhileCollection: BidirectionalCollection
where Base: BidirectionalCollection {
  @inlinable // FIXME(sil-serialize-all)
  public func index(before i: Index) -> Index {
    switch i._value {
    case .index(let i):
      _precondition(i != _base.startIndex, "Can't move before startIndex")
      return Index(_base.index(before: i))
    case .pastEnd:
      // Look for the position of the last element in a non-empty
      // prefix(while:) collection by searching forward for a predicate
      // failure.

      // Safe to assume that `_base.startIndex != _base.endIndex`; if they
      // were equal, `_base.startIndex` would be used as the `endIndex` of
      // this collection.
      _sanityCheck(!_base.isEmpty)
      var result = _base.startIndex
      while true {
        let next = _base.index(after: result)
        if next == _base.endIndex || !_predicate(_base[next]) {
          break
        }
        result = next
      }
      return Index(result)
    }
  }
}

extension LazyPrefixWhileCollection: LazyCollectionProtocol {
  public typealias Elements = LazyPrefixWhileCollection
}

extension LazyCollectionProtocol {
  /// Returns a lazy collection of the initial consecutive elements that
  /// satisfy `predicate`.
  ///
  /// - Parameter predicate: A closure that takes an element of the collection
  ///   as its argument and returns `true` if the element should be included
  ///   or `false` otherwise. Once `predicate` returns `false` it will not be
  ///   called again.
  @inlinable // FIXME(sil-serialize-all)
  public func prefix(
    while predicate: @escaping (Element) -> Bool
  ) -> LazyPrefixWhileCollection<Elements> {
    return LazyPrefixWhileCollection(
      _base: self.elements, predicate: predicate)
  }
}

@available(*, deprecated, renamed: "LazyDropWhileSequence.Iterator")
public typealias LazyPrefixWhileIterator<T> = LazyPrefixWhileSequence<T>.Iterator where T: Sequence
@available(*, deprecated, renamed: "LazyDropWhileCollection.Index")
public typealias LazyPrefixWhileIndex<T> = LazyPrefixWhileCollection<T>.Index where T: Collection
@available(*, deprecated, renamed: "LazyPrefixWhileCollection")
public typealias LazyPrefixWhileBidirectionalCollection<T> = LazyPrefixWhileCollection<T> where T: BidirectionalCollection
