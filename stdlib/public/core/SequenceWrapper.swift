//===--- SequenceWrapper.swift - sequence/collection wrapper protocols ----===//
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
//
//  To create a Sequence that forwards requirements to an
//  underlying Sequence, have it conform to this protocol.
//
//===----------------------------------------------------------------------===//

/// A type that is just a wrapper over some base Sequence
@_show_in_interface
public // @testable
protocol _SequenceWrapper {
  associatedtype Base : Sequence
  associatedtype Iterator : IteratorProtocol = Base.Iterator
  
  var _base: Base { get }
}

extension _SequenceWrapper where
  Self : Sequence,
  Self.Iterator == Self.Base.Iterator {

  /// Returns a value less than or equal to the number of elements in
  /// the sequence, nondestructively.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the sequence if the
  ///   sequence is a collection or wraps a collection; otherwise, O(1).
  public var underestimatedCount: Int {
    return _base.underestimatedCount
  }
}

extension Sequence
  where
  Self : _SequenceWrapper,
  Self.Iterator == Self.Base.Iterator {

  /// Returns an iterator over the elements of this sequence.
  public func makeIterator() -> Base.Iterator {
    return self._base.makeIterator()
  }

  /// Returns an array containing the results of mapping the given closure
  /// over the sequence's elements.
  ///
  /// In this example, `map` is used first to convert the names in the array to
  /// lowercase strings and then to count their characters.
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let lowercaseNames = cast.map { $0.lowercaseString }
  ///     // 'lowercaseNames' == ["vivien", "marlon", "kim", "karl"]
  ///     let letterCounts = cast.map { $0.characters.count }
  ///     // 'letterCounts' == [6, 6, 3, 4]
  ///
  /// - Parameter transform: A mapping closure. `transform` accepts an
  ///   element of this sequence as its parameter and returns a transformed
  ///   value of the same or of a different type.
  /// - Returns: An array containing the transformed elements of this
  ///   sequence.
  public func map<T>(
    _ transform: (Base.Iterator.Element) throws -> T
  ) rethrows -> [T] {
    return try _base.map(transform)
  }

  /// Returns an array containing, in order, the elements of the sequence
  /// that satisfy the given predicate.
  ///
  /// In this example, `filter` is used to include only names shorter than five
  /// characters.
  ///
  ///     let cast = ["Vivien", "Marlon", "Kim", "Karl"]
  ///     let shortNames = cast.filter { $0.characters.count < 5 }
  ///     print(shortNames)
  ///     // Prints "["Kim", "Karl"]"
  ///
  /// - Parameter includeElement: A closure that takes an element of the
  ///   sequence as its argument and returns a Boolean value indicating
  ///   whether the element should be included in the returned array.
  /// - Returns: An array of the elements that `includeElement` allowed.
  public func filter(
    _ isIncluded: (Base.Iterator.Element) throws -> Bool
  ) rethrows -> [Base.Iterator.Element] {
    return try _base.filter(isIncluded)
  }
  
  public func _customContainsEquatableElement(
    _ element: Base.Iterator.Element
  ) -> Bool? { 
    return _base._customContainsEquatableElement(element)
  }
  
  /// If `self` is multi-pass (i.e., a `Collection`), invoke
  /// `preprocess` on `self` and return its result.  Otherwise, return
  /// `nil`.
  public func _preprocessingPass<R>(
    _ preprocess: () throws -> R
  ) rethrows -> R? {
    return try _base._preprocessingPass(preprocess)
  }

  /// Create a native array buffer containing the elements of `self`,
  /// in the same order.
  public func _copyToContiguousArray()
    -> ContiguousArray<Base.Iterator.Element> {
    return _base._copyToContiguousArray()
  }

  /// Copy a Sequence into an array, returning one past the last
  /// element initialized.
  @discardableResult
  public func _copyContents(
    initializing buf: UnsafeMutableBufferPointer<Base.Iterator.Element>
  ) -> (Base.Iterator,UnsafeMutableBufferPointer<Base.Iterator.Element>.Index) {
    return _base._copyContents(initializing: buf)
  }
}
