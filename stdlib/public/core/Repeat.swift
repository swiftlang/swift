//===--- Repeated.swift - A Collection that repeats a value N times -------===//
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

/// A collection whose elements are all identical `Element`s.
public struct Repeated<Element> : Collection {
  /// A type that represents a valid position in the collection.
  /// 
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Int

  /// Construct an instance that contains `length` elements having the
  /// value `repeatedValue`.
  internal init(_repeating repeatedValue: Element, length: Int) {
    self.length = length
    self.repeatedValue = repeatedValue
  }
  
  /// Always zero, which is the index of the first element in a
  /// non-empty instance.
  public var startIndex: Index {
    return 0
  }

  /// Always equal to `length`, which is one greater than the index of
  /// the last element in a non-empty instance.
  public var endIndex: Index {
    return length
  }

  /// Access the element at `position`.
  ///
  /// - Requires: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Int) -> Element {
    _require(position >= 0 && position < length, "Index out of range")
    return repeatedValue
  }

  /// The number of elements in this collection.
  public let length: Int

  /// The value of every element in this collection.
  public let repeatedValue: Element
}

/// Return a collection containing `n` repetitions of `elementInstance`.
public func repeatElement<T>(element: T, count n: Int) -> Repeated<T> {
  return Repeated(_repeating: element, length: n)
}

@available(*, unavailable, renamed="Repeated")
public struct Repeat<Element> {}

extension Repeated {
  @available(*, unavailable, renamed="repeatElement")
  public init(count: Int, repeatedValue: Element) {
    fatalError("unavailable function can't be called")
  }
}

