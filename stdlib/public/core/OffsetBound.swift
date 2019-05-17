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

/// A position in a collection specified as an offset from either the start
/// or the end of the collection (i.e. the collection's bounds).
///
/// You can use an `OffsetBound` to access an index or element of a collection
/// as well as extract a slice. For example:
///
///       let str = "abcdefghijklmnopqrstuvwxyz"
///       print(str[.last]) // Optional("z")
///       print(str[.last - 2]) // Optional("x")
///       print(str[.start + 26]) // nil
///       print(str[.start + 3 ..< .start + 6]) // "def"
///       print(str[.start + 3 ..< .end - 3]) // "defghijklmnopqrstuvw"
///
/// `OffsetBound`s also provide a convenient way of working with slice types
/// over collections whose index type is `Int`. Slice types share indices with
/// their base collection, so `0` doesn't always mean the first element. For
/// example:
///
///     let array = [1,2,3,4,5,6]
///     print(array[2...][3) // 4
///     print(array[2...][.start + 3]!) // 6
///
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
public struct OffsetBound {
  internal enum Kind {
    case fromStart(Int)
    case fromEnd(Int)
  }
  internal var kind: Kind
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension OffsetBound {
  internal init(fromStart: Int) {
    self.kind = .fromStart(fromStart)
  }

  internal init(fromEnd: Int) {
    self.kind = .fromEnd(fromEnd)
  }

  internal func advanced(by: Int) -> OffsetBound {
    switch self.kind {
      case .fromStart(let offset): return OffsetBound(fromStart: offset + by)
      case .fromEnd(let offset): return OffsetBound(fromEnd: offset + by)
    }
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension OffsetBound {

  /// The position corresponding to `startIndex` and the first element of a
  /// nonempty collection.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static var start: OffsetBound { return OffsetBound(fromStart: 0) }

  /// The position corresponding to `endIndex`, which is "past the end"---that
  /// is, the position one greater than the last valid subscript argument.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static var end: OffsetBound { return OffsetBound(fromEnd: 0) }

  /// The position corresponding to `startIndex` and the first element of a
  /// nonempty collection. This is equivalent to `start`.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static var first: OffsetBound { return start }

  /// The position corresponding to the last element of an nonempty
  /// collection. This is equivalent to `.end - 1`.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static var last: OffsetBound { return OffsetBound(fromEnd: -1) }

  /// Returns a bound that offsets the given bound by the specified distance.
  ///
  /// For example:
  ///
  ///     .first + 2  // The position of the 3rd element
  ///     .last + 1   // One past the last element, equivalent to `.end`
  ///
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static func +(_ lhs: OffsetBound, _ rhs: Int) -> OffsetBound {
    return lhs.advanced(by: rhs)
  }

  /// Returns a bound that offsets the given bound by the specified distance
  /// backwards.
  ///
  /// For example:
  ///
  ///     .last - 2 // Two positions before the last element's position
  ///     .end - 1  // The position before the (open) upper-bound, i.e. `.last`
  ///
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static func -(_ lhs: OffsetBound, _ rhs: Int) -> OffsetBound {
    return lhs.advanced(by: -rhs)
  }
}

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
extension OffsetBound: Comparable {

  /// Compare the positions represented by two `OffsetBound`s.
  ///
  /// Offsets relative to `.start` are always less than those relative to
  /// `.end`, as there are arbitrarily many offsets between the two extremities.
  /// Offsets from the same bound are ordered by their corresponding positions.
  /// For example:
  ///
  ///     .start + n < .end - m    // true for all values of n and m
  ///     .start + n < .start + m  // equivalent to n < m
  ///     .end - n < .end - m      // equivalent to n > m
  ///
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static func < (_ lhs: OffsetBound, _ rhs: OffsetBound) -> Bool {
    switch (lhs.kind, rhs.kind) {
    case (.fromStart(_), .fromEnd(_)): return true
    case (.fromEnd(_), .fromStart(_)): return false
    case (.fromStart(let lhs), .fromStart(let rhs)): return lhs < rhs
    case (.fromEnd(let lhs), .fromEnd(let rhs)): return lhs < rhs
    }
  }

  /// Compare two `OffsetBound`s to see if they represent equivalent positions.
  ///
  /// This is only true if both offset the same bound by the same amount. For
  /// example:
  ///
  ///     .last == .end - 1         // true
  ///     .start + n == .end - m    // false for all values of n and m
  ///     .start + n == .start + m  // equivalent to n == m
  ///
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public static func == (_ lhs: OffsetBound, _ rhs: OffsetBound) -> Bool {
    switch (lhs.kind, rhs.kind) {
    case (.fromStart(_), .fromEnd(_)): return false
    case (.fromEnd(_), .fromStart(_)): return false
    case (.fromStart(let lhs), .fromStart(let rhs)): return lhs == rhs
    case (.fromEnd(let lhs), .fromEnd(let rhs)): return lhs == rhs
    }
  }
}

extension Collection {
  /// Returns the corresponding index for the provided offset, if it exists,
  /// else returns nil.
  ///
  /// - Complexity:
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the offset if the collection conforms to
  ///     `BidirectionalCollection`.
  ///   - O(*k*) if the offset is positive or zero.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  ///   Note that `.last` represents `.end - 1`, therefore it has a negative
  ///   offset
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public func index(at bound: OffsetBound) -> Index? {
    switch bound.kind {
      case .fromStart(let int):
        guard int >= 0 else { return nil }
        return self.index(
          self.startIndex, offsetBy: int, limitedBy: self.endIndex)
      case .fromEnd(let int):
        guard int <= 0 else { return nil }
        return self._reverseOffsetIndex(self.endIndex, by: -int)
    }
  }

  // Returns an index for this collection, clamping to startIndex and endIndex
  // if out of bounds in the respective direction.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  internal func _clampedIndex(at bound: OffsetBound) -> Index {
    switch bound.kind {
      case .fromStart(let int):
        guard int >= 0 else { return self.startIndex }
        return self.index(
          self.startIndex, offsetBy: int, limitedBy: self.endIndex
        ) ?? self.endIndex
      case .fromEnd(let int):
        guard int <= 0 else { return endIndex }
        return self._reverseOffsetIndex(
          self.endIndex, by: -int
        ) ?? self.startIndex
    }
  }
}

// To get a Range<OffsetBound> from a RangeExpression<OffsetBound>
@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
private struct OffsetBoundConverter: Collection {
  fileprivate var startIndex: OffsetBound { return .start }
  fileprivate var endIndex: OffsetBound { return .end }

  fileprivate func index(after bound: OffsetBound) -> OffsetBound {
    return bound.advanced(by: 1)
  }

  fileprivate subscript(bound: OffsetBound) -> OffsetBound { return bound }
  fileprivate subscript(bounds: Range<OffsetBound>) -> Range<OffsetBound> {
    return bounds
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

extension Collection {
  /// Returns the corresponding element for the provided offset, if it exists,
  /// else returns nil.
  ///
  /// Example:
  ///
  ///       let str = "abcdefghijklmnopqrstuvwxyz"
  ///       print(str[.last]) // Optional("z")
  ///       print(str[.last - 2]) // Optional("x")
  ///       print(str[.start + 26]) // nil
  ///
  /// - Complexity:
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the offset if the collection conforms to
  ///     `BidirectionalCollection`.
  ///   - O(*k*) if the offset is positive or zero.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  ///   Note that `.last` represents `.end - 1`, therefore it has a negative
  ///   offset
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public subscript(bound: OffsetBound) -> Element? {
    guard let idx = self.index(at: bound), idx != endIndex else { return nil }
    return self[idx]
  }

  /// Returns the contiguous subrange of elements corresponding to the provided
  /// offsets.
  ///
  /// Example:
  ///
  ///       let str = "abcdefghijklmnopqrstuvwxyz"
  ///       print(str[.start + 3 ..< .start + 6]) // "def"
  ///       print(str[.start + 3 ..< .end - 3]) // "defghijklmnopqrstuvw"
  ///
  /// - Complexity:
  ///   - O(1) if the collection conforms to `RandomAccessCollection`.
  ///   - O(*k*) where *k* is equal to the larger offset if the collection
  ///     conforms to `BidirectionalCollection`.
  ///   - O(*k*) if both offsets are positive or zero.
  ///   - Otherwise, O(*n*) where *n* is the length of the collection.
  ///   Note that `.last` represents `.end - 1`, therefore it has a negative
  ///   offset
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *)
  public subscript<ORE: RangeExpression>(
    range: ORE
  ) -> SubSequence where ORE.Bound == OffsetBound {
    return self[range._relative(to: self)]
  }
}
