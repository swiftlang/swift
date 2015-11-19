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
//  To create a Sequence or Collection that forwards
//  requirements to an underlying Sequence or Collection,
//  have it conform to one of these protocols.
//
//===----------------------------------------------------------------------===//

/// A type that is just a wrapper over some base Sequence
public // @testable
protocol _SequenceWrapper {
  typealias Base : Sequence
  typealias Iterator : IteratorProtocol = Base.Iterator
  
  var _base: Base {get}
}

extension Sequence
  where
  Self : _SequenceWrapper,
  Self.Iterator == Self.Base.Iterator {

  /// Return an *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> Base.Iterator {
    return self._base.iterator()
  }

  public func underestimatedLength() -> Int {
    return _base.underestimatedLength()
  }

  @warn_unused_result
  public func map<T>(
    @noescape transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    return try _base.map(transform)
  }

  @warn_unused_result
  public func filter(
    @noescape includeElement: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    return try _base.filter(includeElement)
  }
  
  public func _customContainsEquatableElement(
    element: Base.Iterator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(preprocess: (Self)->R) -> R? {
    return _base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Iterator.Element> {
    return _base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array, returning one past the last
  /// element initialized.
  public func _initializeTo(ptr: UnsafeMutablePointer<Base.Iterator.Element>)
    -> UnsafeMutablePointer<Base.Iterator.Element> {
    return _base._initializeTo(ptr)
  }
}

public // @testable
protocol _CollectionWrapper : _SequenceWrapper {
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
  /// - Requires: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Base.Index) -> Base.Iterator.Element {
    return _base[position]
  }

  //===--- Restatements From SequenceWrapperType break ambiguity ----------===//
  @warn_unused_result
  public func map<T>(
    @noescape transform: (Base.Iterator.Element) -> T
  ) -> [T] {
    return _base.map(transform)
  }

  @warn_unused_result
  public func filter(
    @noescape includeElement: (Base.Iterator.Element) -> Bool
  ) -> [Base.Iterator.Element] {
    return _base.filter(includeElement)
  }
  
  public func _customContainsEquatableElement(
    element: Base.Iterator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(preprocess: (Self)->R) -> R? {
    return _base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Iterator.Element> {
    return _base._copyToNativeArrayBuffer()
  }

  /// Copy a Sequence into an array.
  public func _initializeTo(ptr: UnsafeMutablePointer<Base.Iterator.Element>)
    -> UnsafeMutablePointer<Base.Iterator.Element> {
    return _base._initializeTo(ptr)
  }
}
