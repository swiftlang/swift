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
public // @testable
protocol _SequenceWrapperType {
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

  public func map<T>(
    @noescape transform: (Base.Generator.Element) -> T
  ) -> [T] {
    return _base.map(transform)
  }

  public func filter(
    @noescape includeElement: (Base.Generator.Element) -> Bool
  ) -> [Base.Generator.Element] {
    return _base.filter(includeElement)
  }
  
  public func _customContainsEquatableElement(
    element: Base.Generator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `CollectionType`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(preprocess: (Self)->R) -> R? {
    return _base._preprocessingPass { _ in preprocess(self) }
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToNativeArrayBuffer()
    -> _ContiguousArrayBuffer<Base.Generator.Element> {
    return _base._copyToNativeArrayBuffer()
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
