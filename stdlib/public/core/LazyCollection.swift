//===--- LazyCollection.swift ---------------------------------*- swift -*-===//
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

/// A collection on which normally-eager operations such as `map` and
/// `filter` are implemented lazily.
///
/// Please see `LazySequenceType` for background; `LazyCollectionType`
/// is an analogous component, but for collections.
///
/// To add new lazy collection operations, extend this protocol with
/// methods that return lazy wrappers that are themselves
/// `LazyCollectionType`s.
///
/// - See Also: `LazySequenceType`, `LazyCollection`
public protocol LazyCollectionType
  : CollectionType, LazySequenceType {
  /// A `CollectionType` that can contain the same elements as this one,
  /// possibly with a simpler type.
  ///
  /// - See also: `elements`
  typealias Elements: CollectionType = Self

}

/// When there's no special associated `Elements` type, the `elements`
/// property is provided.
extension LazyCollectionType where Elements == Self {
  /// Identical to `self`.
  public var elements: Self { return self }
}

/// A collection containing the same elements as a `Base` collection,
/// but on which some operations such as `map` and `filter` are
/// implemented lazily.
///
/// - See also: `LazySequenceType`, `LazyCollection`
public struct LazyCollection<Base : CollectionType>
  : LazyCollectionType {

  /// The type of the underlying collection
  public typealias Elements = Base

  /// The underlying collection
  public var elements: Elements { return _base }

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Base.Index
  
  /// Construct an instance with `base` as its underlying Collection
  /// instance.
  public init(_ base: Base) {
    self._base = base
  }

  internal var _base: Base
}

/// Forward implementations to the base collection, to pick up any
/// optimizations it might implement.
extension LazyCollection : SequenceType {
  
  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> Base.Generator { return _base.generate() }
  
  /// Return a value less than or equal to the number of elements in
  /// `self`, **nondestructively**.
  ///
  /// - Complexity: O(N).
  public func underestimateCount() -> Int { return _base.underestimateCount() }

  public func _copyToNativeArrayBuffer() 
     -> _ContiguousArrayBuffer<Base.Generator.Element> {
    return _base._copyToNativeArrayBuffer()
  }
  
  public func _initializeTo(
    ptr: UnsafeMutablePointer<Base.Generator.Element>
  ) -> UnsafeMutablePointer<Base.Generator.Element> {
    return _base._initializeTo(ptr)
  }

  public func _customContainsEquatableElement(
    element: Base.Generator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
}

extension LazyCollection : CollectionType {
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

  /// Returns a collection representing a contiguous sub-range of
  /// `self`'s elements.
  ///
  /// - Complexity: O(1)
  public subscript(bounds: Range<Index>) -> LazyCollection<Slice<Base>> {
    return Slice(base: _base, bounds: bounds).lazy
  }
  
  /// Returns `true` iff `self` is empty.
  public var isEmpty: Bool {
    return _base.isEmpty
  }

  /// Returns the number of elements.
  ///
  /// - Complexity: O(1) if `Index` conforms to `RandomAccessIndexType`;
  ///   O(N) otherwise.
  public var count: Index.Distance {
    return _base.count
  }
  
  // The following requirement enables dispatching for indexOf when
  // the element type is Equatable.
  
  /// Returns `Optional(Optional(index))` if an element was found;
  /// `nil` otherwise.
  ///
  /// - Complexity: O(N).
  public func _customIndexOfEquatableElement(
    element: Base.Generator.Element
  ) -> Index?? {
    return _base._customIndexOfEquatableElement(element)
  }

  /// Returns the first element of `self`, or `nil` if `self` is empty.
  public var first: Base.Generator.Element? {
    return _base.first
  }

  @available(*, unavailable, renamed="Base")
  public typealias S = Void
}

/// Augment `self` with lazy methods such as `map`, `filter`, etc.
extension CollectionType {
  /// A collection with contents identical to `self`, but on which
  /// normally-eager operations such as `map` and `filter` are
  /// implemented lazily.
  ///
  /// - See Also: `LazySequenceType`, `LazyCollectionType`.
  public var lazy: LazyCollection<Self> {
    return LazyCollection(self)
  }
}

extension LazyCollectionType {
  /// Identical to `self`.
  public var lazy: Self { // Don't re-wrap already-lazy collections
    return self
  }
}

@available(*, unavailable, message="Please use the collection's '.lazy' property")
public func lazy<Base : CollectionType>(s: Base) -> LazyCollection<Base> {
  fatalError("unavailable")
}

@available(*, unavailable, renamed="LazyCollection")
public struct LazyForwardCollection<T> {}
@available(*, unavailable, renamed="LazyCollection")
public struct LazyBidirectionalCollection<T> {}
@available(*, unavailable, renamed="LazyCollection")
public struct LazyRandomAccessCollection<T> {}

// ${'Local Variables'}:
// eval: (read-only-mode 1)
// End:
