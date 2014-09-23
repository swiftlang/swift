//===--- CollectionOfOne.swift - A CollectionType with one element --------===//
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

/// A *generator* that produces one or fewer instances of `T`.
public struct GeneratorOfOne<T> : GeneratorType, SequenceType {
  public init(_ elements: T?) {
    self.elements = elements
  }

  /// `GeneratorOfOne` is also a `SequenceType`, so it `generate`\
  /// 's a copy of itself
  public func generate() -> GeneratorOfOne {
    return self
  }
  
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: `next()` has not been applied to a copy of `self`
  /// since the copy was made, and no preceding call to `self.next()`
  /// has returned `nil`.
  public mutating func next() -> T? {
    let result = elements
    elements = .None
    return result
  }
  var elements: T?
}

public struct CollectionOfOne<T> : CollectionType {
  /// A type that represents a valid position in the collection.
  /// 
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Bit

  public init(_ element: T) { 
    self.element = element 
  }

  /// The position of the first element.
  public var startIndex: Index {
    return .Zero
  }

  /// The "past the end" position; always identical to
  /// `startIndex.successor()`.
  ///
  /// Note: `endIndex` is not a valid argument to `subscript`.
  public var endIndex: Index {
    return .One
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> GeneratorOfOne<T> {
    return GeneratorOfOne(element)
  }

  /// Access the element at `position`.
  ///
  /// Requires: `position == .Zero`
  public subscript(position: Index) -> T {
    _precondition(position == .Zero, "Index out of range")
    return element
  }
  
  let element: T
}

// Specialization of countElements for CollectionOfOne<T>
public func ~> <T>(x:CollectionOfOne<T>, _:(_CountElements, ())) -> Int {
  return 1
}
