//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A position in a collection specified as an offset from either the first
/// or last element of the collection (i.e. the collection's bounds).
///
/// You can use an `OffsetBound` to access an index or element of a collection
/// as well as extract a slice. For example:
///
///       let str = "abcdefghijklmnopqrstuvwxyz"
///       print(str[.last]) // Optional("z")
///       print(str[.last - 2]) // Optional("x")
///       print(str[.first + 26]) // nil
///       print(str[.first + 3 ..< .first + 6]) // "def"
///       print(str[.first + 3 ..< .last - 2]) // "defghijklmnopqrstuvw"
///
/// `OffsetBound`s also provide a convenient way of working with slice types
/// over collections whose index type is `Int`. Slice types share indices with
/// their base collection, so `0` doesn't always mean the first element. For
/// example:
///
///     let array = [1,2,3,4,5,6]
///     print(array[2...][3) // 4
///     print(array[2...][.first + 3]!) // 6
///
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
public struct OffsetBound {
  internal enum Anchor {
    case fromStart
    case fromEnd
  }
  internal var anchor: Anchor
  internal var offset: Int
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension OffsetBound {
  internal init(fromStart: Int) {
    self.init(anchor: .fromStart, offset: fromStart)
  }

  internal init(fromEnd: Int) {
    self.init(anchor: .fromEnd, offset: fromEnd)
  }

  internal func advanced(by: Int) -> OffsetBound {
    OffsetBound(anchor: self.anchor, offset: self.offset + by)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension OffsetBound {
  /// The position of the first element of a nonempty collection, corresponding
  /// to `startIndex`.
  public static var first: OffsetBound { OffsetBound(fromStart: 0) }

  /// The position of the last element of a nonempty collection, corresponding
  /// to `index(before: endIndex)`.
  public static var last: OffsetBound { OffsetBound(fromEnd: -1) }

  /// Returns a bound that offsets the given bound by the specified distance.
  ///
  /// For example:
  ///
  ///     .first + 2  // The position of the 3rd element
  ///     .last + 1   // One past the last element, corresponding to `endIndex`
  ///
  public static func +(_ lhs: OffsetBound, _ rhs: Int) -> OffsetBound {
    lhs.advanced(by: rhs)
  }

  /// Returns a bound that offsets the given bound by the specified distance
  /// backwards.
  ///
  /// For example:
  ///
  ///     .last - 2 // Two positions before the last element's position
  ///
  public static func -(_ lhs: OffsetBound, _ rhs: Int) -> OffsetBound {
    lhs.advanced(by: -rhs)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension OffsetBound: Comparable {

  /// Compare the positions represented by two `OffsetBound`s.
  ///
  /// Offsets relative to `.first` are always less than those relative to
  /// `.last`, as there are arbitrarily many offsets between the two
  /// extremities. Offsets from the same bound are ordered by their
  /// corresponding positions. For example:
  ///
  ///     .first + n < .last - m    // true for all values of n and m
  ///     .first + n < .first + m  // equivalent to n < m
  ///     .last - n < .last - m      // equivalent to n > m
  ///
  public static func < (_ lhs: OffsetBound, _ rhs: OffsetBound) -> Bool {
    if lhs.anchor == rhs.anchor { return lhs.offset < rhs.offset }

    return lhs.anchor == .fromStart
  }

  /// Compare two `OffsetBound`s to see if they represent equivalent positions.
  ///
  /// This is only true if both offset the same bound by the same amount. For
  /// example:
  ///
  ///     .first + n == .last - m    // false for all values of n and m
  ///     .first + n == .first + m  // equivalent to n == m
  ///
  public static func == (_ lhs: OffsetBound, _ rhs: OffsetBound) -> Bool {
    lhs.anchor == rhs.anchor && lhs.offset == rhs.offset
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension Collection {
  /// Returns the corresponding index for the provided offset, if it exists,
  /// else returns nil.
  ///
  /// - Complexity:
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the offset if the collection conforms to
  ///     `BidirectionalCollection`.
  ///   - O(*k*) if `position` is `.first + n` for any n, or `.last + 1`.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  public func index(at position: OffsetBound) -> Index? {
    switch position.anchor {
      case .fromStart:
        guard position.offset >= 0 else { return nil }

        return self.index(
          self.startIndex, offsetBy: position.offset, limitedBy: self.endIndex)

      case .fromEnd:
        guard position.offset <= 0 else { return nil }

        return self._reverseOffsetIndex(self.endIndex, by: -position.offset)
    }
  }

  // Returns an index for this collection, clamping to startIndex and endIndex
  // if out of bounds in the respective direction.
  internal func _clampedIndex(at bound: OffsetBound) -> Index {
    switch bound.anchor {
      case .fromStart:
        guard bound.offset >= 0 else { return self.startIndex }

        return self.index(
          self.startIndex, offsetBy: bound.offset, limitedBy: self.endIndex
        ) ?? self.endIndex

      case .fromEnd:
        guard bound.offset <= 0 else { return endIndex }

        return self._reverseOffsetIndex(
          self.endIndex, by: -bound.offset
        ) ?? self.startIndex
    }
  }
}

// To get a Range<OffsetBound> from a RangeExpression<OffsetBound>
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
private struct OffsetBoundConverter: Collection {
  fileprivate var startIndex: OffsetBound { .first }
  fileprivate var endIndex: OffsetBound { .last + 1 }

  fileprivate func index(after bound: OffsetBound) -> OffsetBound {
    bound.advanced(by: 1)
  }

  fileprivate subscript(bound: OffsetBound) -> OffsetBound { bound }
  fileprivate subscript(bounds: Range<OffsetBound>) -> Range<OffsetBound> {
    bounds
  }

  fileprivate init() { }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension RangeExpression where Bound == OffsetBound {
  internal func _relative<C: Collection>(to c: C) -> Range<C.Index> {
    let range = self.relative(to: OffsetBoundConverter())
    let lower = c._clampedIndex(at: range.lowerBound)
    let upper = c._clampedIndex(at: range.upperBound)
    return lower ..< max(lower, upper)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension Collection {
  internal func _subscriptGet(_ bound: OffsetBound) -> Element? {
    guard let idx = self.index(at: bound), idx != endIndex else { return nil }
    return self[idx]
  }

  internal func _subscriptGet<ORE: RangeExpression>(
    _ range: ORE
  ) -> SubSequence where ORE.Bound == OffsetBound {
    self[range._relative(to: self)]
  }

  /// Returns the corresponding element for the provided offset, if it exists,
  /// else returns nil.
  ///
  /// Example:
  ///
  ///       let abcs = "abcdefg"
  ///       print(abcs[.last]) // Optional("g")
  ///       print(abcs[.last - 2]) // Optional("e")
  ///       print(abcs[.first + 8]) // nil
  ///
  /// - Complexity:
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the offset if the collection conforms to
  ///     `BidirectionalCollection`.
  ///   - O(*k*) if `position` is `.first + n` for any n, or `.last + 1`.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  public subscript(position: OffsetBound) -> Element? {
    _subscriptGet(position)
  }

  /// Returns the contiguous subrange of elements corresponding to the provided
  /// offsets.
  ///
  /// Example:
  ///
  ///       let abcs = "abcdefg"
  ///       print(abcs[.first + 1 ..< .first + 6]) // "bcdef"
  ///       print(abcs[.first + 1 ..< .last - 1]) // "bcde"
  ///
  /// - Complexity:
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the larger offset if the collection
  ///     conforms to `BidirectionalCollection`.
  ///   - O(*k*) if the offsets are `.first + n` for any n or `.last + 1`.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  public subscript<ORE: RangeExpression>(
    range: ORE
  ) -> SubSequence where ORE.Bound == OffsetBound {
    _subscriptGet(range)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension RangeReplaceableCollection {
  /// Replaces the specified subrange of elements with the given collection.
  ///
  /// This method has the effect of removing the specified range of elements
  /// from the collection and inserting the new elements at the same location.
  /// The number of new elements need not match the number of elements being
  /// removed.
  ///
  /// In this example, two characters in the middle of a string are
  /// replaced by the three elements of a `Repeated<Character>` instance.
  ///
  ///      var animals = "🐕🐈🐱🐩"
  ///      let dogFaces = repeatElement("🐶" as Character, count: 3)
  ///      animals.replaceSubrange(.first + 1 ... .last - 1, with: dogFaces)
  ///      print(animals)
  ///      // Prints "🐕🐶🐶🐶🐩"
  ///
  /// If you pass a zero-length range as the `subrange` parameter, this method
  /// inserts the elements of `newElements` at `subrange.startIndex`. Calling
  /// the `insert(contentsOf:at:)` method instead is preferred.
  ///
  /// Likewise, if you pass a zero-length collection as the `newElements`
  /// parameter, this method removes the elements in the given subrange
  /// without replacement. Calling the `removeSubrange(_:)` method instead is
  /// preferred.
  ///
  /// Calling this method may invalidate any existing indices for use with this
  /// collection.
  ///
  /// - Parameters:
  ///   - subrange: The subrange of the collection to replace, specified as
  ///   offsets from the collection's bounds.
  ///   - newElements: The new elements to add to the collection.
  ///
  /// - Complexity: O(*n* + *m*), where *n* is length of this collection and
  ///   *m* is the length of `newElements`. If the call to this method simply
  ///   appends the contents of `newElements` to the collection, the complexity
  ///   is O(*m*).
  public mutating func replaceSubrange<C: Collection, R: RangeExpression>(
    _ subrange: R, with newElements: __owned C
  ) where C.Element == Element, R.Bound == OffsetBound {
    self.replaceSubrange(subrange._relative(to: self), with: newElements)
  }

  /// Inserts a new element into the collection at the specified position.
  ///
  /// The new element is inserted before the element currently at the specified
  /// offset. If you pass `.last + 1` as the `position` parameter, corresponding
  /// to the collection's `endIndex`, the new element is appended to the
  /// collection.
  ///
  ///     var numbers = "12345"
  ///     numbers.insert("Ⅸ", at: .first + 1)
  ///     numbers.insert("𐄕", at: .last + 1)
  ///
  ///     print(numbers)
  ///     // Prints "1Ⅸ2345𐄕"
  ///
  /// Calling this method may invalidate any existing indices for use with this
  /// collection.
  ///
  /// - Parameter newElement: The new element to insert into the collection.
  /// - Parameter `position`: The position at which to insert the new element,
  ///   specified as offsets from the collection's bounds
  ///
  /// - Complexity: O(*n*), where *n* is the length of the collection. If
  ///   `position == .last + 1`, this method is equivalent to `append(_:)`.
  public mutating func insert(
    _ newElement: __owned Element, at position: OffsetBound
  ) {
    self.insert(newElement, at: self._clampedIndex(at: position))
  }

  /// Inserts the elements of a sequence into the collection at the specified
  /// position.
  ///
  /// The new elements are inserted before the element currently at the
  /// specified offset. If you pass `.last + 1` as the `position` parameter,
  /// corresponding to the collection's `endIndex`, the new elements are
  /// appended to the collection.
  ///
  /// Here's an example of inserting vulgar fractions in a string of numbers.
  ///
  ///     var numbers = "12345"
  ///     numbers.insert(contentsOf: "↉⅖⅑", at: .first + 2)
  ///     print(numbers)
  ///     // Prints "12↉⅖⅑345"
  ///
  /// Calling this method may invalidate any existing indices for use with this
  /// collection.
  ///
  /// - Parameter newElements: The new elements to insert into the collection.
  /// - Parameter `position`: The position at which to insert the new elements,
  ///   specified as offsets from the collection's bounds
  ///
  /// - Complexity: O(*n* + *m*), where *n* is length of this collection and
  ///   *m* is the length of `newElements`. If `position == .last + 1`, this
  ///   method is equivalent to `append(contentsOf:)`.
  public mutating func insert<S: Collection>(
    contentsOf newElements: __owned S, at position: OffsetBound
  ) where S.Element == Element {
    self.insert(contentsOf: newElements, at: self._clampedIndex(at: position))
  }

  /// Removes and returns the element at the specified position, if it exists,
  /// else returns nil.
  ///
  /// All the elements following the specified position are moved to close the
  /// gap.
  ///
  /// Example:
  ///     var measurements = [1.2, 1.5, 2.9, 1.2, 1.6]
  ///     let removed = measurements.remove(at: .last - 2)
  ///     print(measurements)
  ///     // Prints "[1.2, 1.5, 1.2, 1.6]"
  ///     print(measurements.remove(at: .first + 4))
  ///     // Prints nil
  ///
  /// Calling this method may invalidate any existing indices for use with this
  /// collection.
  ///
  /// - Parameter position: The position of the element to remove, specified as
  ///   an offset from the collection's bounds.
  /// - Returns: The removed element if it exists, else nil
  ///
  /// - Complexity: O(*n*), where *n* is the length of the collection.
  public mutating func remove(at position: OffsetBound) -> Element? {
    guard let idx = self.index(at: position), idx != endIndex else { return nil }
    return self.remove(at: idx)
  }

  /// Removes the elements in the specified subrange from the collection.
  ///
  /// All the elements following the specified position are moved to close the
  /// gap. This example removes two elements from the middle of a string of
  /// rulers.
  ///
  ///     var rulers = "📏🤴👑📐"
  ///     rulers.removeSubrange(.first + 1 ... .last - 1)
  ///     print(rulers)
  ///     // Prints "📏📐"
  ///
  /// Calling this method may invalidate any existing indices for use with this
  /// collection.
  ///
  /// - Parameter range: The range of the collection to be removed, specified
  ///   as offsets from the collection's bounds.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the collection.
  public mutating func removeSubrange<R: RangeExpression>(
    _ range: R
  ) where R.Bound == OffsetBound {
    self.removeSubrange(range._relative(to: self))
  }

  /// Accesses the element corresponding to the provided offset. If no element
  /// exists, `nil` is returned from the getter. Similarly, setting an element
  /// to `nil` will remove the element at that offset.
  ///
  /// Example:
  ///
  ///       let abcs = "abcdefg"
  ///       print(abcs[.last]) // Optional("g")
  ///       print(abcs[.last - 2]) // Optional("e")
  ///       print(abcs[.first + 8]) // nil
  ///       abcs[.first + 2] = "©"
  ///       print(abcs) // "ab©defg"
  ///       abcs[.last - 1] = nil
  ///       print(abcs) // "ab©deg"
  ///
  /// - Complexity (get):
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the offset if the collection conforms to
  ///     `BidirectionalCollection`.
  ///   - O(*k*) if `position` is `.first + n` for any n, or `.last + 1`.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  ///
  /// - Complexity (set):
  ///   - O(*n*) where *n* is the length of the collection.
  public subscript(position: OffsetBound) -> Element? {
    get { return _subscriptGet(position) }
    set {
      guard let elt = newValue else {
        _ = self.remove(at: position)
        return
      }
      self.replaceSubrange(position ..< position + 1, with: CollectionOfOne(elt))
    }
  }

  /// Accesses the contiguous subrange of elements corresponding to the provided
  /// offsets.
  ///
  /// Example:
  ///
  ///       var abcs = "abcdefg"
  ///       print(abcs[.first + 1 ..< .first + 6]) // "bcdef"
  ///       print(abcs[.first + 1 ..< .last - 1]) // "bcde"
  ///       abcs[.first ... .first + 3] = "🔡"
  ///       print(abcs) // "🔡efg"
  ///
  /// - Complexity (get):
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the larger offset if the collection
  ///     conforms to `BidirectionalCollection`.
  ///   - O(*k*) if the offsets are `.first + n` for any n or `.last + 1`.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  ///
  /// - Complexity (set):
  ///   - O(*n*) where *n* is the length of the collection.
  public subscript<ORE: RangeExpression>(
    range: ORE
  ) -> SubSequence where ORE.Bound == OffsetBound {
    get { return _subscriptGet(range) }
    set { self.replaceSubrange(range, with: newValue) }
  }
}


