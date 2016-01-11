//===--- Map.swift - Lazily map over a SequenceType -----------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// The `GeneratorType` used by `MapSequence` and `MapCollection`.
/// Produces each element by passing the output of the `Base`
/// `GeneratorType` through a transform function returning `Element`.
public struct LazyMapGenerator<
  Base : GeneratorType, Element
> : GeneratorType, SequenceType {
  @available(*, unavailable, renamed="Element")
  public typealias T = Element

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
  internal var _transform: (Base.Element) -> Element
}

/// A `SequenceType` whose elements consist of those in a `Base`
/// `SequenceType` passed through a transform function returning `Element`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct LazyMapSequence<Base : SequenceType, Element>
  : LazySequenceType {
  
  public typealias Elements = LazyMapSequence
  
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> LazyMapGenerator<Base.Generator, Element> {
    return LazyMapGenerator(_base: _base.generate(), _transform: _transform)
  }

  /// Return a value less than or equal to the number of elements in
  /// `self`, **nondestructively**.
  ///
  /// - Complexity: O(N).
  public func underestimateCount() -> Int {
    return _base.underestimateCount()
  }

  /// Create an instance with elements `transform(x)` for each element
  /// `x` of base.
  public init(_ base: Base, transform: (Base.Generator.Element) -> Element) {
    self._base = base
    self._transform = transform
  }
  
  public var _base: Base
  internal var _transform: (Base.Generator.Element) -> Element

  @available(*, unavailable, renamed="Element")
  public typealias T = Element
}

//===--- Collections ------------------------------------------------------===//

/// A `CollectionType` whose elements consist of those in a `Base`
/// `CollectionType` passed through a transform function returning `Element`.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
public struct LazyMapCollection<Base : CollectionType, Element>
  : LazyCollectionType {

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
  

  /// Returns a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> LazyMapGenerator<Base.Generator, Element> {
    return LazyMapGenerator(_base: _base.generate(), _transform: _transform)
  }

  public func underestimateCount() -> Int {
    return _base.underestimateCount()
  }

  /// Returns the number of elements.
  ///
  /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
  ///   O(N) otherwise.
  public var count: Base.Index.Distance {
    return _base.count
  }

  /// Create an instance with elements `transform(x)` for each element
  /// `x` of base.
  public init(_ base: Base, transform: (Base.Generator.Element) -> Element) {
    self._base = base
    self._transform = transform
  }
  
  public var _base: Base
  var _transform: (Base.Generator.Element) -> Element

  @available(*, unavailable, renamed="Element")
  public typealias T = Element
}

//===--- Support for s.lazy -----------------------------------------------===//

extension LazySequenceType {
  /// Return a `LazyMapSequence` over this `Sequence`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  @warn_unused_result
  public func map<U>(
    transform: (Elements.Generator.Element) -> U
  ) -> LazyMapSequence<Self.Elements, U> {
    return LazyMapSequence(self.elements, transform: transform)
  }
}

extension LazyCollectionType {
  /// Return a `LazyMapCollection` over this `Collection`.  The elements of
  /// the result are computed lazily, each time they are read, by
  /// calling `transform` function on a base element.
  @warn_unused_result
  public func map<U>(
    transform: (Elements.Generator.Element) -> U
  ) -> LazyMapCollection<Self.Elements, U> {
    return LazyMapCollection(self.elements, transform: transform)
  }
}

/// Return an `Array` containing the results of mapping `transform`
/// over `source`.
@available(*, unavailable, message="call the 'map()' method on the sequence")
public func map<C : CollectionType, T>(
  source: C, _ transform: (C.Generator.Element) -> T
) -> [T] {
  fatalError("unavailable function can't be called")
}

/// Return an `Array` containing the results of mapping `transform`
/// over `source` and flattening the result.
@available(*, unavailable, message="call the 'flatMap()' method on the sequence")
public func flatMap<C : CollectionType, T>(
  source: C, _ transform: (C.Generator.Element) -> [T]
) -> [T] {
  fatalError("unavailable function can't be called")
}

@available(*, unavailable, renamed="LazyMapGenerator")
public struct MapSequenceGenerator<Base : GeneratorType, T> {}

@available(*, unavailable, renamed="LazyMapSequence")
public struct MapSequenceView<Base : SequenceType, T> {}

@available(*, unavailable, renamed="LazyMapCollection")
public struct MapCollectionView<Base : CollectionType, T> {}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
