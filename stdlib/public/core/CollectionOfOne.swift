//===--- CollectionOfOne.swift - A Collection with one element ------------===//
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

/// An iterator that produces one or fewer instances of `Element`.
@_fixed_layout // FIXME(sil-serialize-all)
public struct IteratorOverOne<Element> {
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _elements: Element?

  /// Construct an instance that generates `_element!`, or an empty
  /// sequence if `_element == nil`.
  @inlinable // FIXME(sil-serialize-all)
  public // @testable
  init(_elements: Element?) {
    self._elements = _elements
  }
}

extension IteratorOverOne: IteratorProtocol, Sequence {
  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  ///
  /// - Precondition: `next()` has not been applied to a copy of `self`
  ///   since the copy was made.
  @inlinable // FIXME(sil-serialize-all)
  public mutating func next() -> Element? {
    let result = _elements
    _elements = nil
    return result
  }
}

/// A collection containing a single element of type `Element`.
@_fixed_layout // FIXME(sil-serialize-all)
public struct CollectionOfOne<Element> {
  @usableFromInline // FIXME(sil-serialize-all)
  internal var _element: Element

  /// Creates an instance containing just `element`.
  @inlinable // FIXME(sil-serialize-all)
  public init(_ element: Element) {
    self._element = element
  }
}

extension CollectionOfOne: RandomAccessCollection, MutableCollection {

  public typealias Index = Int
  public typealias Indices = Range<Int>

  /// The position of the first element.
  @inlinable // FIXME(sil-serialize-all)
  public var startIndex: Int {
    return 0
  }

  /// The "past the end" position---that is, the position one greater than the
  /// last valid subscript argument.
  ///
  /// In a `CollectionOfOne` instance, `endIndex` is always identical to
  /// `index(after: startIndex)`.
  @inlinable // FIXME(sil-serialize-all)
  public var endIndex: Int {
    return 1
  }
  
  /// Always returns `endIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public func index(after i: Int) -> Int {
    _precondition(i == startIndex)
    return endIndex
  }

  /// Always returns `startIndex`.
  @inlinable // FIXME(sil-serialize-all)
  public func index(before i: Int) -> Int {
    _precondition(i == endIndex)
    return startIndex
  }

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @inlinable // FIXME(sil-serialize-all)
  public func makeIterator() -> IteratorOverOne<Element> {
    return IteratorOverOne(_elements: _element)
  }

  /// Accesses the element at `position`.
  ///
  /// - Precondition: `position == 0`.
  @inlinable // FIXME(sil-serialize-all)
  public subscript(position: Int) -> Element {
    get {
      _precondition(position == 0, "Index out of range")
      return _element
    }
    set {
      _precondition(position == 0, "Index out of range")
      _element = newValue
    }
  }

  @inlinable // FIXME(sil-serialize-all)
  public subscript(bounds: Range<Int>)
    -> Slice<CollectionOfOne<Element>> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      _precondition(bounds.count == newValue.count,
        "CollectionOfOne can't be resized")
      if let newElement = newValue.first {
        _element = newElement
      }
    }
  }

  /// The number of elements (always one).
  @inlinable // FIXME(sil-serialize-all)
  public var count: Int {
    return 1
  }
}

extension CollectionOfOne : CustomDebugStringConvertible {
  /// A textual representation of `self`, suitable for debugging.
  @inlinable // FIXME(sil-serialize-all)
  public var debugDescription: String {
    return "CollectionOfOne(\(String(reflecting: _element)))"
  }
}

extension CollectionOfOne : CustomReflectable {
  @inlinable // FIXME(sil-serialize-all)
  public var customMirror: Mirror {
    return Mirror(self, children: ["element": _element])
  }
}
