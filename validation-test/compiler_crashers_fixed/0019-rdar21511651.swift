// RUN: not %target-swift-frontend %s -typecheck

internal protocol _SequenceWrapper {
  typealias Base : Sequence
  typealias Iterator : IteratorProtocol = Base.Iterator
  
  var _base: Base {get}
}

extension Sequence
  where Self : _SequenceWrapper, Self.Iterator == Self.Base.Iterator {
  public func makeIterator() -> Base.Iterator {
    return self._base.makeIterator()
  }

  public func underestimatedCount() -> Int {
    return _base.underestimatedCount()
  }

  public func _customContainsEquatableElement(
    element: Base.Iterator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(_ preprocess: (Self) -> R) -> R? {
    return _base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Iterator.Element> {
    return _base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array.
  public func _copyContents(
    initializing ptr: UnsafeMutablePointer<Base.Iterator.Element>
  ) {
    return _base._copyContents(initializing: ptr)
  }
}

internal protocol _CollectionWrapper : _SequenceWrapper {
  typealias Base : Collection
  typealias Index : ForwardIndex = Base.Index
  var _base: Base {get}
}

extension Collection
  where Self : _CollectionWrapper, Self.Index == Self.Base.Index {
  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  public var startIndex: Base.Index {
    return _base.startIndex
  }
  
  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: Base.Index {
    return _base.endIndex
  }

  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Base.Index) -> Base.Iterator.Element {
    return _base[position]
  }
}

//===--- New stuff --------------------------------------------------------===//
public protocol _prext_LazySequence : Sequence {
  /// A Sequence that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// This associated type is used to keep the result type of
  /// `lazy(x).operation` from growing a `_prext_LazySequence` layer.
  typealias Elements: Sequence = Self

  /// A sequence containing the same elements as this one, possibly with
  /// a simpler type.
  ///
  /// When implementing lazy operations, wrapping `elements` instead
  /// of `self` can prevent result types from growing a `_prext_LazySequence`
  /// layer.
  ///
  /// Note: this property need not be implemented by conforming types,
  /// it has a default implementation in a protocol extension that
  /// just returns `self`.
  var elements: Elements {get} 
  
  /// An Array, created on-demand, containing the elements of this
  /// lazy Sequence.
  ///
  /// Note: this property need not be implemented by conforming types, it has a
  /// default implementation in a protocol extension.
  var array: [Iterator.Element] {get}
}

extension _prext_LazySequence {
  /// an Array, created on-demand, containing the elements of this
  /// lazy Sequence.
  public var array: [Iterator.Element] {
    return Array(self)
  }
}

extension _prext_LazySequence where Elements == Self {
  public var elements: Self { return self }
}

extension _prext_LazySequence where Self : _SequenceWrapper {
  public var elements: Base { return _base }
}

/// A sequence that forwards its implementation to an underlying
/// sequence instance while exposing lazy computations as methods.
public struct _prext_LazySequence<Base_ : Sequence> : _SequenceWrapper {
  var _base: Base_
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
public func _prext_lazy<S : Sequence>(s: S) -> _prext_LazySequence<S> {
  return _prext_LazySequence(_base: s)
}

public extension Sequence
  where Self.Iterator == Self, Self : IteratorProtocol {
  public func makeIterator() -> Self {
    return self
  }
}

//===--- LazyCollection.swift ---------------------------------------------===//
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

public protocol _prext_LazyCollection : Collection, _prext_LazySequence {
  /// A Collection that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// This associated type is used to keep the result type of
  /// `lazy(x).operation` from growing a `_prext_LazyCollection` layer.
  typealias Elements: Collection = Self

}

extension _prext_LazyCollection where Elements == Self {
  public var elements: Self { return self }
}

extension _prext_LazyCollection where Self : _CollectionWrapper {
  public var elements: Base { return _base }
}

/// A collection that forwards its implementation to an underlying
/// collection instance while exposing lazy computations as methods.
public struct _prext_LazyCollection<Base_ : Collection>
  : /*_prext_LazyCollection,*/ _CollectionWrapper {

  typealias Base = Base_
  typealias Index = Base.Index
  
  /// Construct an instance with `base` as its underlying Collection
  /// instance.
  public init(_ base: Base_) {
    self._base = base
  }

  public var _base: Base_

  // FIXME: Why is this needed?
  // public var elements: Base { return _base }
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
public func _prext_lazy<Base: Collection>(s: Base) -> _prext_LazyCollection<Base> {
  return _prext_LazyCollection(s)
}

//===--- New stuff --------------------------------------------------------===//
/// The `IteratorProtocol` used by `_prext_MapSequence` and `_prext_MapCollection`.
/// Produces each element by passing the output of the `Base`
/// `IteratorProtocol` through a transform function returning `T`.
public struct _prext_MapIterator<
  Base: IteratorProtocol, T
> : IteratorProtocol, Sequence {
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Precondition: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> T? {
    let x = _base.next()
    if x != nil {
      return _transform(x!)
    }
    return nil
  }

  var _base: Base
  var _transform: (Base.Element)->T
}

//===--- Sequences --------------------------------------------------------===//

/// A `Sequence` whose elements consist of those in a `Base`
/// `Sequence` passed through a transform function returning `T`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct _prext_MapSequence<Base : Sequence, T>
  : _prext_LazySequence, _SequenceWrapper {

  typealias Elements = _prext_MapSequence

  public func makeIterator() -> _prext_MapIterator<Base.Iterator,T> {
    return _prext_MapIterator(
      _base: _base.makeIterator(), _transform: _transform)
  }

  var _base: Base
  var _transform: (Base.Iterator.Element)->T
}

//===--- Collections ------------------------------------------------------===//

/// A `Collection` whose elements consist of those in a `Base`
/// `Collection` passed through a transform function returning `T`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct _prext_MapCollection<Base : Collection, T>
  : _prext_LazyCollection, _CollectionWrapper {

  public var startIndex: Base.Index { return _base.startIndex }
  public var endIndex: Base.Index { return _base.endIndex }
  
  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Base.Index) -> T {
    return _transform(_base[position])
  }

  public func makeIterator() -> _prext_MapIterator<Base.Iterator, T> {
    return _prext_MapIterator(_base: _base.makeIterator(), _transform: _transform)
  }

  public func underestimatedCount() -> Int {
    return _base.underestimatedCount()
  }

  var _base: Base
  var _transform: (Base.Iterator.Element)->T
}

//===--- Support for lazy(s) ----------------------------------------------===//

extension _prext_LazySequence {
  /// Return a `_prext_MapSequence` over this `Sequence`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  public func map<U>(
    transform: (Elements.Iterator.Element) -> U
  ) -> _prext_MapSequence<Self.Elements, U> {
    return _prext_MapSequence(_base: self.elements, _transform: transform)
  }
}

extension _prext_LazyCollection {
  /// Return a `_prext_MapCollection` over this `Collection`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  public func map<U>(
    transform: (Elements.Iterator.Element) -> U
  ) -> _prext_MapCollection<Self.Elements, U> {
    return _prext_MapCollection(_base: self.elements, _transform: transform)
  }
}

 // ${'Local Variables'}:
 // eval: (read-only-mode 1)
 // End:
 
//===--- New stuff --------------------------------------------------------===//
internal protocol __prext_ReverseCollection : _prext_LazyCollection {
  typealias Base : Collection
  var _base : Base {get}
}


/// A wrapper for a `BidirectionalIndex` that reverses its
/// direction of traversal.
public struct _prext_ReverseIndex<I : BidirectionalIndex> : BidirectionalIndex {
  var _base: I

  init(_ _base: I) { self._base = _base }

  /// Returns the next consecutive value after `self`.
  ///
  /// - Precondition: The next value is representable.
  public func successor() -> _prext_ReverseIndex {
    return _prext_ReverseIndex(_base.predecessor())
  }

  /// Returns the previous consecutive value before `self`.
  ///
  /// - Precondition: The previous value is representable.
  public func predecessor() -> _prext_ReverseIndex {
    return _prext_ReverseIndex(_base.successor())
  }

  /// A type that can represent the number of steps between pairs of
  /// `_prext_ReverseIndex` values where one value is reachable from the other.
  typealias Distance = I.Distance
}

/// A wrapper for a `${IndexProtocol}` that reverses its
/// direction of traversal.
public struct _prext_ReverseRandomAccessIndex<I : RandomAccessIndex> : RandomAccessIndex {
  var _base: I

  init(_ _base: I) { self._base = _base }

  /// Returns the next consecutive value after `self`.
  ///
  /// - Precondition: The next value is representable.
  public func successor() -> _prext_ReverseRandomAccessIndex {
    return _prext_ReverseRandomAccessIndex(_base.predecessor())
  }

  /// Returns the previous consecutive value before `self`.
  ///
  /// - Precondition: The previous value is representable.
  public func predecessor() -> _prext_ReverseRandomAccessIndex {
    return _prext_ReverseRandomAccessIndex(_base.successor())
  }

  /// A type that can represent the number of steps between pairs of
  /// `_prext_ReverseRandomAccessIndex` values where one value is reachable from the other.
  typealias Distance = I.Distance

  /// Return the minimum number of applications of `successor` or
  /// `predecessor` required to reach `other` from `self`.
  ///
  /// - Complexity: O(1).
  public func distance(to other: _prext_ReverseRandomAccessIndex) -> Distance {
    return other._base.distance(to: _base)
  }

  /// Return `self` offset by `n` steps.
  ///
  /// - Returns: If `n > 0`, the result of applying `successor` to
  ///   `self` `n` times.  If `n < 0`, the result of applying
  ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
  ///
  /// - Complexity: O(1).
  public func advanced(by amount: Distance) -> _prext_ReverseRandomAccessIndex {
    return _prext_ReverseRandomAccessIndex(_base.advanced(by: -amount))
  }
}

public func == <I> (lhs: _prext_ReverseIndex<I>, rhs: _prext_ReverseIndex<I>) -> Bool {
  return lhs._base == rhs._base
}

public func == <I> (lhs: _prext_ReverseRandomAccessIndex<I>, rhs: _prext_ReverseRandomAccessIndex<I>) -> Bool {
  return lhs._base == rhs._base
}

extension Collection
  where Self : __prext_ReverseCollection, Self.Base.Index : BidirectionalIndex {
  public var startIndex : _prext_ReverseIndex<Base.Index> {
    return _prext_ReverseIndex<Base.Index>(_base.endIndex)
  }
  public var endIndex : _prext_ReverseIndex<Base.Index> {
    return _prext_ReverseIndex<Base.Index>(_base.startIndex)
  }
  public subscript(position: _prext_ReverseIndex<Base.Index>) -> Base.Iterator.Element {
    return _base[position._base.predecessor()]
  }
}


extension Collection
  where Self : __prext_ReverseCollection, Self.Base.Index : RandomAccessIndex {
  public var startIndex : _prext_ReverseRandomAccessIndex<Base.Index> {
    return _prext_ReverseRandomAccessIndex<Base.Index>(_base.endIndex)
  }
  public var endIndex : _prext_ReverseRandomAccessIndex<Base.Index> {
    return _prext_ReverseRandomAccessIndex<Base.Index>(_base.startIndex)
  }
  public subscript(position: _prext_ReverseRandomAccessIndex<Base.Index>) -> Base.Iterator.Element {
    return _base[position._base.predecessor()]
  }
}





/// The lazy `Collection` returned by `c.reversed()` where `c` is a
/// `Collection` with an `Index` conforming to `${IndexProtocol}`.
public struct _prext_ReverseCollection<Base : Collection>
  : Collection, __prext_ReverseCollection {

  public init(_ _base: Base) {
    self._base = _base
  }

  internal var _base: Base
}
