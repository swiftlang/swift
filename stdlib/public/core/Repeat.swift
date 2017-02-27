//===--- Repeat.swift - A Collection that repeats a value N times ---------===//
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

/// A collection whose elements are all identical.
///
/// You create an instance of the `Repeated` collection by calling the
/// `repeatElement(_:count:)` function. The following example creates a
/// collection containing the name "Humperdinck" repeated five times:
///
///     let repeatedName = repeatElement("Humperdinck", count: 5)
///     for name in repeatedName {
///         print(name)
///     }
///     // "Humperdinck"
///     // "Humperdinck"
///     // "Humperdinck"
///     // "Humperdinck"
///     // "Humperdinck"
public struct Repeated<Element> : RandomAccessCollection {

  public typealias Indices = CountableRange<Int>

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a "past the
  /// end" position that's not valid for use as a subscript.
  public typealias Index = Int

  /// Creates an instance that contains `count` elements having the
  /// value `repeatedValue`.
  internal init(_repeating repeatedValue: Element, count: Int) {
    _precondition(count >= 0, "Repetition count should be non-negative")
    self.count = count
    self.repeatedValue = repeatedValue
  }

  /// The position of the first element in a nonempty collection.
  ///
  /// In a `Repeated` collection, `startIndex` is always equal to zero. If the
  /// collection is empty, `startIndex` is equal to `endIndex`.
  public var startIndex: Index {
    return 0
  }

  /// The collection's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// In a `Repeated` collection, `endIndex` is always equal to `count`. If the
  /// collection is empty, `endIndex` is equal to `startIndex`.
  public var endIndex: Index {
    return count
  }

  /// Accesses the element at the specified position.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the collection that is not equal to the
  ///   `endIndex` property.
  public subscript(position: Int) -> Element {
    _precondition(position >= 0 && position < count, "Index out of range")
    return repeatedValue
  }

  /// The number of elements in this collection.
  public let count: Int

  /// The value of every element in this collection.
  public let repeatedValue: Element
}

/// Creates a collection containing the specified number of the given element.
///
/// The following example creates a `Repeated<Int>` collection containing five
/// zeroes:
///
///     let zeroes = repeatElement(0, count: 5)
///     for x in zeroes {
///         print(x)
///     }
///     // 0
///     // 0
///     // 0
///     // 0
///     // 0
///
/// - Parameters:
///   - element: The element to repeat.
///   - count: The number of times to repeat `element`.
/// - Returns: A collection that contains `count` elements that are all
///   `element`.
public func repeatElement<T>(_ element: T, count n: Int) -> Repeated<T> {
  return Repeated(_repeating: element, count: n)
}

@available(*, unavailable, renamed: "Repeated")
public struct Repeat<Element> {}

extension Repeated {
  @available(*, unavailable, message: "Please use repeatElement(_:count:) function instead")
  public init(count: Int, repeatedValue: Element) {
    Builtin.unreachable()
  }
}
