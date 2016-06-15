//===--- Repeat.swift - A Collection that repeats a value N times ---------===//
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
public struct Repeated<Element> : RandomAccessCollection {

  public typealias Indices = CountableRange<Int>

  /// A type that represents a valid position in the collection.
  /// 
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Int

  /// Construct an instance that contains `count` elements having the
  /// value `repeatedValue`.
  internal init(_repeating repeatedValue: Element, count: Int) {
    _precondition(count >= 0, "Repetition count should be non-negative")
    self.count = count
    self.repeatedValue = repeatedValue
  }
  
  /// Always zero, which is the index of the first element in a
  /// non-empty instance.
  public var startIndex: Index {
    return 0
  }

  /// Always equal to `count`, which is one greater than the index of
  /// the last element in a non-empty instance.
  public var endIndex: Index {
    return count
  }

  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Int) -> Element {
    _precondition(position >= 0 && position < count, "Index out of range")
    return repeatedValue
  }

  /// The number of elements in this collection.
  public let count: Int

  /// The value of every element in this collection.
  public let repeatedValue: Element
}

/// Return a collection containing `n` repetitions of `elementInstance`.
public func repeatElement<T>(_ element: T, count n: Int) -> Repeated<T> {
  return Repeated(_repeating: element, count: n)
}

@available(*, unavailable, renamed: "Repeated")
public struct Repeat<Element> {}

extension Repeated {
  @available(*, unavailable, renamed: "repeatElement")
  public init(count: Int, repeatedValue: Element) {
    Builtin.unreachable()
  }
}
