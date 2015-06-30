// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

import StdlibUnittest

//===--- SequenceWrapper.swift - sequence/collection wrapper protocols ----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  To create a SequenceType or CollectionType that forwards
//  requirements to an underlying SequenceType or CollectionType,
//  have it conform to one of these protocols.
//
//===----------------------------------------------------------------------===//

/// A type that is just a wrapper over some base Sequence
internal protocol _SequenceWrapperType {
  typealias Base : SequenceType
  typealias Generator : GeneratorType = Base.Generator
  
  var _base: Base {get}
}

extension SequenceType
  where Self : _SequenceWrapperType, Self.Generator == Self.Base.Generator {
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> Base.Generator {
    return self._base.generate()
  }

  public func underestimateCount() -> Int {
    return _base.underestimateCount()
  }

  public func _customContainsEquatableElement(
    element: Base.Generator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  /// Copy a Sequence into an array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Base.Generator.Element>) {
    return _base._initializeTo(ptr)
  }
}

internal protocol _CollectionWrapperType : _SequenceWrapperType {
  typealias Base : CollectionType
  typealias Index : ForwardIndexType = Base.Index
  var _base: Base {get}
}

extension CollectionType
  where Self : _CollectionWrapperType, Self.Index == Self.Base.Index {
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
  /// - Requires: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Base.Index) -> Base.Generator.Element {
    return _base[position]
  }
}

//===--- New stuff --------------------------------------------------------===//
public protocol _prext_LazySequenceType : SequenceType {
  /// A SequenceType that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// This associated type is used to keep the result type of
  /// `lazy(x).operation` from growing a `_prext_LazySequence` layer.
  typealias Elements: SequenceType = Self

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
  /// lazy SequenceType.
  ///
  /// Note: this property need not be implemented by conforming types, it has a
  /// default implementation in a protocol extension.
  var array: [Generator.Element] {get}
}

extension _prext_LazySequenceType {
  /// an Array, created on-demand, containing the elements of this
  /// lazy SequenceType.
  public var array: [Generator.Element] {
    return Array(self)
  }
}

extension _prext_LazySequenceType where Elements == Self {
  public var elements: Self { return self }
}

extension _prext_LazySequenceType where Self : _SequenceWrapperType {
  public var elements: Base { return _base }
}

/// A sequence that forwards its implementation to an underlying
/// sequence instance while exposing lazy computations as methods.
public struct _prext_LazySequence<Base_ : SequenceType> : _SequenceWrapperType {
  var _base: Base_
}

/// Augment `s` with lazy methods such as `map`, `filter`, etc.
public func _prext_lazy<S : SequenceType>(s: S) -> _prext_LazySequence<S> {
  return _prext_LazySequence(_base: s)
}

public extension SequenceType
  where Self.Generator == Self, Self : GeneratorType {
  public func generate() -> Self {
    return self
  }
}

//===--- LazyCollection.swift ---------------------------------*- swift -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

public protocol _prext_LazyCollectionType : CollectionType, _prext_LazySequenceType {
  /// A CollectionType that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// This associated type is used to keep the result type of
  /// `lazy(x).operation` from growing a `_prext_LazyCollection` layer.
  typealias Elements: CollectionType = Self

}

extension _prext_LazyCollectionType where Elements == Self {
  public var elements: Self { return self }
}

extension _prext_LazyCollectionType where Self : _CollectionWrapperType {
  public var elements: Base { return _base }
}

/// A collection that forwards its implementation to an underlying
/// collection instance while exposing lazy computations as methods.
public struct _prext_LazyCollection<Base_ : CollectionType>
  : /*_prext_LazyCollectionType,*/ _CollectionWrapperType {

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
public func _prext_lazy<Base: CollectionType>(s: Base) -> _prext_LazyCollection<Base> {
  return _prext_LazyCollection(s)
}


//===--- Map.swift --------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

//===--- New stuff --------------------------------------------------------===//
/// The `GeneratorType` used by `_prext_MapSequence` and `_prext_MapCollection`.
/// Produces each element by passing the output of the `Base`
/// `GeneratorType` through a transform function returning `T`.
public struct _prext_MapGenerator<
  Base: GeneratorType, T
> : GeneratorType, SequenceType {
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
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

/// A `SequenceType` whose elements consist of those in a `Base`
/// `SequenceType` passed through a transform function returning `T`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct _prext_MapSequence<Base : SequenceType, T>
  : _prext_LazySequenceType, _SequenceWrapperType {

  typealias Elements = _prext_MapSequence
  
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> _prext_MapGenerator<Base.Generator,T> {
    return _prext_MapGenerator(
      _base: _base.generate(), _transform: _transform)
  }

  var _base: Base
  var _transform: (Base.Generator.Element)->T
}

//===--- Collections ------------------------------------------------------===//

/// A `CollectionType` whose elements consist of those in a `Base`
/// `CollectionType` passed through a transform function returning `T`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct _prext_MapCollection<Base : CollectionType, T>
  : _prext_LazyCollectionType, _CollectionWrapperType {

  public var startIndex: Base.Index { return _base.startIndex }
  public var endIndex: Base.Index { return _base.endIndex }
  
  /// Access the element at `position`.
  ///
  /// - Requires: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Base.Index) -> T {
    return _transform(_base[position])
  }

  /// Returns a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> _prext_MapGenerator<Base.Generator, T> {
    return _prext_MapGenerator(_base: _base.generate(), _transform: _transform)
  }

  public func underestimateCount() -> Int {
    return _base.underestimateCount()
  }

  var _base: Base
  var _transform: (Base.Generator.Element)->T
}

//===--- Support for lazy(s) ----------------------------------------------===//

extension _prext_LazySequenceType {
  /// Return a `_prext_MapSequence` over this `Sequence`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  public func map<U>(
    transform: (Elements.Generator.Element) -> U
  ) -> _prext_MapSequence<Self.Elements, U> {
    return _prext_MapSequence(_base: self.elements, _transform: transform)
  }
}

extension _prext_LazyCollectionType {
  /// Return a `_prext_MapCollection` over this `Collection`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  public func map<U>(
    transform: (Elements.Generator.Element) -> U
  ) -> _prext_MapCollection<Self.Elements, U> {
    return _prext_MapCollection(_base: self.elements, _transform: transform)
  }
}

 // ${'Local Variables'}:
 // eval: (read-only-mode 1)
 // End:
 
//===--- New stuff --------------------------------------------------------===//

internal protocol _prext_ReverseIndexType : BidirectionalIndexType {
  typealias Base : BidirectionalIndexType
  
  /// A type that can represent the number of steps between pairs of
  /// `_prext_ReverseIndex` values where one value is reachable from the other.
  typealias Distance: _SignedIntegerType = Base.Distance
  
  var _base: Base { get }
  init(_ base: Base)
}

/// A wrapper for a `BidirectionalIndexType` that reverses its
/// direction of traversal.
extension BidirectionalIndexType where Self : _prext_ReverseIndexType {
  /// Returns the next consecutive value after `self`.
  ///
  /// - Requires: The next value is representable.
  public func successor() -> Self {
    return Self(_base.predecessor())
  }

  /// Returns the previous consecutive value before `self`.
  ///
  /// - Requires: The previous value is representable.
  public func predecessor() -> Self {
    return Self(_base.successor())
  }
}

public struct _prext_ReverseIndex<Base_: BidirectionalIndexType>
: BidirectionalIndexType, _prext_ReverseIndexType {
  typealias Base = Base_
  typealias Distance = Base_.Distance
  
  internal init(_ base: Base) { self._base = base }
  internal let _base: Base
}

public func == <I> (lhs: _prext_ReverseIndex<I>, rhs: _prext_ReverseIndex<I>) -> Bool {
  return lhs._base == rhs._base
}

public struct _prext_ReverseRandomAccessIndex<Base: RandomAccessIndexType>
  : RandomAccessIndexType, _prext_ReverseIndexType {

  typealias Distance = Base.Distance
  
  internal init(_ base: Base) { self._base = base }
  internal let _base: Base

  /// Return the minimum number of applications of `successor` or
  /// `predecessor` required to reach `other` from `self`.
  ///
  /// - Complexity: O(1).
  public func distanceTo(other: _prext_ReverseRandomAccessIndex) -> Distance {
    return other._base.distanceTo(_base)
  }

  /// Return `self` offset by `n` steps.
  ///
  /// - Returns: If `n > 0`, the result of applying `successor` to
  ///   `self` `n` times.  If `n < 0`, the result of applying
  ///   `predecessor` to `self` `-n` times. Otherwise, `self`.
  ///
  /// - Complexity: O(1).
  public func advancedBy(amount: Distance) -> _prext_ReverseRandomAccessIndex {
    return _prext_ReverseRandomAccessIndex(_base.advancedBy(-amount))
  }
}

internal protocol _ReverseCollectionType : _prext_LazyCollectionType, _CollectionWrapperType {
  typealias Index : _prext_ReverseIndexType
}

extension CollectionType
  where Self : _ReverseCollectionType, Self.Base.Index : RandomAccessIndexType {
  public var startIndex : _prext_ReverseRandomAccessIndex<Self.Base.Index> {
    return _prext_ReverseRandomAccessIndex(_base.endIndex)
  }
}

extension CollectionType
  where Self : _ReverseCollectionType, Self.Base.Index : BidirectionalIndexType {
  public var startIndex : _prext_ReverseIndex<Self.Base.Index> {
    return _prext_ReverseIndex(_base.endIndex)
  }
}

extension CollectionType
  where Self : _ReverseCollectionType, Self.Index.Base == Self.Base.Index
{
  public var endIndex : Index { return Self.Index(_base.startIndex) }
  public subscript(position: Index) -> Self.Base.Generator.Element {
    return _base[position._base.predecessor()]
  }
}

public struct _prext_ReverseCollection<
  Base_ : CollectionType where Base_.Index : BidirectionalIndexType
> : _ReverseCollectionType {
  
  typealias Index = _prext_ReverseIndex<Base_.Index>
  typealias Generator = IndexingGenerator<_prext_ReverseCollection>
  
  public init(_ base: Base_) {
    self._base = base
  }

  public let _base: Base_
}

print(_prext_ReverseCollection(0..<12).startIndex)
print(_prext_ReverseCollection(0..<12).array)
print(_prext_ReverseCollection("foobar".characters).array)
print(_prext_ReverseCollection("foobar".characters).startIndex)

/*****
/// The lazy `CollectionType` returned by `reverse(c)` where `c` is a
/// `CollectionType` with an `Index` conforming to `${IndexProtocol}`.
public struct _prext_ReverseCollection<Base_ : CollectionType where Base_.Index : BidirectionalIndexType> : _CollectionWrapperType {

  typealias Base = Base_
  typealias Index = _prext_ReverseIndex<Base.Index>
  
  public init(_ base: Base) {
    self._base = base
  }

  public let _base: Base
}

extension _prext_ReverseCollection : __prext_ReverseCollectionType, _prext_LazyCollectionType  {}

extension CollectionType where Index : BidirectionalIndexType {
  /// Return a `_prext_ReverseCollection` over `self`.  
  public func reverse() -> _prext_ReverseCollection<Self> {
    return _prext_ReverseCollection<Self>(
      _prext_ReverseCollection<Self>(self)
    )
  }
}

 // ${'Local Variables'}:
 // eval: (read-only-mode 1)
 // End:


//===--- Filter.swift ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

/// The `GeneratorType` used by `_prext_FilterSequence` and
/// `_prext_FilterCollection`.
public struct _prext_FilterGenerator<
  Base: GeneratorType
> : GeneratorType, SequenceType {
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Base.Element? {
    var n: Base.Element?
    for/*ever*/;; {
      n = _base.next()
      if n != nil ? _include(n!) : true {
        return n
      }
    }
  }

  var _base: Base
  var _include: (Base.Element)->Bool
}

/// The lazy `SequenceType` returned by `filter(c)` where `c` is a
/// `SequenceType`.
public struct _prext_FilterSequence<Base : SequenceType> : _prext_LazySequenceType {
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> _prext_FilterGenerator<Base.Generator> {
    return _prext_FilterGenerator(_base: _base.generate(), _include: _include)
  }

  var _base: Base
  var _include: (Base.Generator.Element)->Bool
}

/// The `Index` used for subscripting a `_prext_FilterCollection`.
///
/// - Note: The performance of advancing a `_prext_FilterIndex` depends on
///   how sparsely the filtering predicate is satisfied, and its
///   conformance to `ForwardIndexType` needs to be understood in that
///   context.
public struct _prext_FilterIndex<
  Base: CollectionType
> : ForwardIndexType {
  /// Returns the next consecutive value after `self`.
  ///
  /// - Requires: The next value is representable.
  /// - Complexity: O(M), where M is the number of following elements
  ///   in the underlying sequence that don't satisfy the predicate.
  public func successor() -> _prext_FilterIndex {
    for nextPos in _pos.successor()..<_end {
      if _include(_base[nextPos]) {
        return _prext_FilterIndex(
          _pos: nextPos, _end: _end,
          _base: _base, _include: _include)
      }
    }
    return _prext_FilterIndex(
      _pos: _end, _end: _end, _base: _base, _include: _include)
  }
  
  internal var _pos: Base.Index
  internal var _end: Base.Index
  internal var _base: Base
  internal var _include: (Base.Generator.Element)->Bool
}

/// Returns `true` iff `lhs` is identical to `rhs`.
public func == <Base: CollectionType>(
  lhs: _prext_FilterIndex<Base>,
  rhs: _prext_FilterIndex<Base>
) -> Bool {
  return lhs._pos == rhs._pos
}

/// A lazy `CollectionType` wrapper that includes the elements of an
/// underlying collection that satisfy a predicate.  Not
/// automatically returned by `filter(x)` for two reasons:
///
/// * The O(1) guarantee of our `Index` would be iffy at best, since
///   it advances an underlying `Index` until the predicate is
///   satisfied.  Be aware that a `_prext_FilterCollection` may not offer
///   the expected efficiency for this reason.
///
/// * Constructing an `Array` from a `CollectionType` measures the length
///   of the collection before traversing it to read the elements.
///   This causes the filter predicate to be called twice for each
///   element of the underlying collection, which is surprising.
public struct _prext_FilterCollection<Base : CollectionType> : _prext_LazyCollectionType {

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = _prext_FilterIndex<Base>

  /// Construct an instance containing the elements of `base` that
  /// satisfy `predicate`.
  public init(
    _ base: Base,
    includeElement predicate: (Base.Generator.Element)->Bool
  ) {
    self._base = base
    self._include = predicate
  }

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  ///
  /// - Complexity: O(N), where N is the ratio between unfiltered and
  ///   filtered collection counts.
  public var startIndex: Index {
    var first = _base.startIndex
    while first != _base.endIndex {
      if _include(_base[first]) {
        break
      }
      ++first
    }
    return _prext_FilterIndex(
      _pos: first, _end: _base.endIndex, _base: _base, _include: _include)
  }

  /// The collection's "past the end" position.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  ///
  /// - Complexity: O(1).
  public var endIndex: Index {
    return _prext_FilterIndex(
      _pos: _base.endIndex, _end: _base.endIndex,
      _base: _base, _include: _include)
  }

  /// Access the element at `position`.
  ///
  /// - Requires: `position` is a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: Index) -> Base.Generator.Element {
    return _base[position._pos]
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> _prext_FilterGenerator<Base.Generator> {
    return _prext_FilterGenerator(_base: _base.generate(), _include: _include)
  }

  var _base: Base
  var _include: (Base.Generator.Element)->Bool
}

extension _prext_LazySequenceType {
  /// Return a `_prext_FilterSequence` that uses this sequence as a base.
  /// The elements to be included in the result are computed lazily,
  /// each time the result is traversed, by calling the
  /// `includeElement` function on elements of the base sequence.
  public func filter(
    includeElement: (Elements.Generator.Element)->Bool
  ) -> _prext_FilterSequence<Self.Elements> {
    return _prext_FilterSequence(_base: self.elements, _include: includeElement)
  }
}
*/

var tests = TestSuite("Lazy")

tests.test("basics") {
  
}

runAllTests()
