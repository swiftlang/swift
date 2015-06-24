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

/// A generator that produces one or fewer instances of `Element`.
public struct GeneratorOfOne<Element> : GeneratorType, SequenceType {
  /// Construct an instance that generates `element!`, or an empty
  /// sequence if `element == nil`.
  public init(_ element: Element?) {
    self.elements = element
  }

  /// `GeneratorOfOne` is also a `SequenceType`, so it `generate`s a
  /// copy of itself.
  public func generate() -> GeneratorOfOne {
    return self
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Element? {
    let result = elements
    elements = .None
    return result
  }
  var elements: Element?
}

/// A collection containing a single element of type `Element`.
public struct CollectionOfOne<Element> : CollectionType {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Bit

  /// Construct an instance containing just `element`.
  public init(_ element: Element) {
    self.element = element
  }

  /// The position of the first element.
  public var startIndex: Index {
    return .Zero
  }

  /// The "past the end" position; always identical to
  /// `startIndex.successor()`.
  ///
  /// - Note: `endIndex` is not a valid argument to `subscript`.
  public var endIndex: Index {
    return .One
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> GeneratorOfOne<Element> {
    return GeneratorOfOne(element)
  }

  /// Access the element at `position`.
  ///
  /// - Requires: `position == .Zero`.
  public subscript(position: Index) -> Element {
    _precondition(position == .Zero, "Index out of range")
    return element
  }

  /// Return the number of elements (always one).
  public var count: Int {
    return 1
  }

  let element: Element
}

