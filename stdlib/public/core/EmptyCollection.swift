//===--- EmptyCollection.swift - A collection with no elements ------------===//
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
//  Sometimes an operation is best expressed in terms of some other,
//  larger operation where one of the parameters is an empty
//  collection.  For example, we can erase elements from an Array by
//  replacing a subrange with the empty collection.
//
//===----------------------------------------------------------------------===//

/// A generator that never produces an element.
///
/// - SeeAlso: `EmptyCollection<T>`.
public struct EmptyGenerator<T> : GeneratorType, SequenceType {
  /// Construct an instance.
  public init() {}

  /// `EmptyGenerator` is also a `SequenceType`, so it `generate`'s
  /// a copy of itself.
  public func generate() -> EmptyGenerator {
    return self
  }

  /// Return `nil`, indicating that there are no more elements.
  public mutating func next() -> T? {
    return nil
  }
}

/// A collection whose element type is `T` but that is always empty.
public struct EmptyCollection<T> : CollectionType {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Int

  /// Construct an instance.
  public init() {}

  /// Always zero, just like `endIndex`.
  public var startIndex: Index {
    return 0
  }

  /// Always zero, just like `startIndex`.
  public var endIndex: Index {
    return 0
  }

  /// Returns an empty *generator*.
  ///
  /// - Complexity: O(1).
  public func generate() -> EmptyGenerator<T> {
    return EmptyGenerator()
  }

  /// Access the element at `position`.
  ///
  /// Should never be called, since this collection is always empty.
  public subscript(position: Index) -> T {
    _preconditionFailure("Index out of range")
  }

  /// Return the number of elements (always zero).
  public var count: Int {
    return 0
  }
}

