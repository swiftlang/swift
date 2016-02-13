//===--- CollectionOfOne.swift - A Collection with one element --------===//
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

/// An iterator that produces one or fewer instances of `Element`.
public struct IteratorOverOne<Element> : IteratorProtocol, Sequence {
  /// Construct an instance that generates `_element!`, or an empty
  /// sequence if `_element == nil`.
  public // @testable
  init(_elements: Element?) {
    self._elements = _elements
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: `next()` has not been applied to a copy of `self`
  ///   since the copy was made, and no preceding call to `self.next()`
  ///   has returned `nil`.
  public mutating func next() -> Element? {
    let result = _elements
    _elements = nil
    return result
  }

  internal var _elements: Element?
}

/// A collection containing a single element of type `Element`.
public struct CollectionOfOne<Element> : Collection {

  /// Construct an instance containing just `element`.
  public init(_ element: Element) {
    self._element = element
  }

  /// The position of the first element.
  public var startIndex: Int {
    return 0
  }

  /// The "past the end" position; always identical to
  /// `startIndex.successor()`.
  ///
  /// - Note: `endIndex` is not a valid argument to `subscript`.
  public var endIndex: Int {
    return 1
  }

  /// Return a *iterator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func iterator() -> IteratorOverOne<Element> {
    return IteratorOverOne(_elements: _element)
  }

  /// Access the element at `position`.
  ///
  /// - Requires: `position == 0`.
  public subscript(position: Int) -> Element {
    _require(position == 0, "Index out of range")
    return _element
  }

  /// Return the number of elements (always one).
  public var count: Int {
    return 1
  }

  internal let _element: Element
}

extension CollectionOfOne : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["element": _element])
  }
}

@available(*, unavailable, renamed="IteratorOverOne")
public struct GeneratorOfOne<Element> {}

extension IteratorOverOne {
  @available(*, unavailable, renamed="iterator")
  public func generate() -> IteratorOverOne<Element> {
    fatalError("unavailable function can't be called")
  }
}
