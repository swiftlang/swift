//===--- SequenceWrapper.swift - sequence/collection wrapper protocols ----===//
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
//
//  To create a Sequence that forwards requirements to an
//  underlying Sequence, have it conform to this protocol.
//
//===----------------------------------------------------------------------===//

/// A type that is just a wrapper over some base Sequence
public // @testable
protocol _SequenceWrapper {
  associatedtype Base : Sequence
  associatedtype Iterator : IteratorProtocol = Base.Iterator
  
  var _base: Base { get }
}

extension _SequenceWrapper where
  Self : Sequence,
  Self.Iterator == Self.Base.Iterator {

  public var underestimatedCount: Int {
    return _base.underestimatedCount
  }
}

extension Sequence
  where
  Self : _SequenceWrapper,
  Self.Iterator == Self.Base.Iterator {

  /// Return an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  public func iterator() -> Base.Iterator {
    return self._base.iterator()
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
  public func _preprocessingPass<R>(@noescape preprocess: () -> R) -> R? {
    return _base._preprocessingPass(preprocess)
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
