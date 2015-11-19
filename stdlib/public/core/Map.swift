//===--- Map.swift - Lazily map over a Sequence -----------*- swift -*-===//
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

/// The `IteratorProtocol` used by `MapSequence` and `MapCollection`.
/// Produces each element by passing the output of the `Base`
/// `IteratorProtocol` through a transform function returning `Element`.
public struct LazyMapIterator<
  Base : IteratorProtocol, Element
> : IteratorProtocol, Sequence {
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Element? {
    return _base.next().map(_transform)
  }

  public var base: Base { return _base }
  
  internal var _base: Base
  internal var _transform: (Base.Element)->Element
}

/// A `Sequence` whose elements consist of those in a `Base`
/// `Sequence` passed through a transform function returning `Element`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct LazyMapSequence<Base : Sequence, Element>
  : LazySequenceProtocol {
  
  public typealias Elements = LazyMapSequence
  
  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> LazyMapIterator<Base.Iterator, Element> {
    return LazyMapIterator(_base: _base.iterator(), _transform: _transform)
  }

  /// Return a value less than or equal to the number of elements in
  /// `self`, **nondestructively**.
  ///
  /// - Complexity: O(N).
  public func underestimatedLength() -> Int {
    return _base.underestimatedLength()
  }

  /// Create an instance with elements `transform(x)` for each element
  /// `x` of base.
  internal init(_ base: Base, transform: (Base.Iterator.Element)->Element) {
    self._base = base
    self._transform = transform
  }
  
  internal var _base: Base
  internal var _transform: (Base.Iterator.Element)->Element
}

//===--- Collections ------------------------------------------------------===//

/// A `Collection` whose elements consist of those in a `Base`
/// `Collection` passed through a transform function returning `Element`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct LazyMapCollection<Base : Collection, Element>
  : LazyCollectionProtocol {

  // FIXME: Should be inferrable.
  public typealias Index = Base.Index

  public var startIndex: Base.Index { return _base.startIndex }
  public var endIndex: Base.Index { return _base.endIndex }
  
  /// Access the element at `position`.
  ///
  /// - Requires: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Base.Index) -> Element {
    return _transform(_base[position])
  }

  /// Returns `true` iff `self` is empty.
  public var isEmpty: Bool { return _base.isEmpty }

  public var first: Element? { return _base.first.map(_transform) }
  

  /// Returns an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> LazyMapIterator<Base.Iterator, Element> {
    return LazyMapIterator(_base: _base.iterator(), _transform: _transform)
  }

  public func underestimatedLength() -> Int {
    return _base.underestimatedLength()
  }

  /// Returns the number of elements.
  ///
  /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndex`;
  ///   O(N) otherwise.
  public var length: Base.Index.Distance {
    return _base.length
  }

  /// Create an instance with elements `transform(x)` for each element
  /// `x` of base.
  internal init(_ base: Base, transform: (Base.Iterator.Element)->Element) {
    self._base = base
    self._transform = transform
  }
  
  internal var _base: Base
  internal var _transform: (Base.Iterator.Element)->Element
}

//===--- Support for s.lazy ----------------------------------------------===//

extension LazySequenceProtocol {
  /// Return a `LazyMapSequence` over this `Sequence`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  @warn_unused_result
  public func map<U>(
    transform: (Elements.Iterator.Element) -> U
  ) -> LazyMapSequence<Self.Elements, U> {
    return LazyMapSequence(self.elements, transform: transform)
  }
}

extension LazyCollectionProtocol {
  /// Return a `LazyMapCollection` over this `Collection`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  @warn_unused_result
  public func map<U>(
    transform: (Elements.Iterator.Element) -> U
  ) -> LazyMapCollection<Self.Elements, U> {
    return LazyMapCollection(self.elements, transform: transform)
  }
}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
