//===--- RangeSet.swift ---------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A set of values of any comparable type, represented by ranges.
///
/// You can use a range set to efficiently represent a set of `Comparable`
/// values that spans any number of discontiguous ranges. Range sets are
/// commonly used to represent multiple subranges of a collection, by storing
/// ranges of a collection's index type.
///
/// In this example, `negativeSubranges` is a range set representing the
/// locations of all the negative values in `numbers`:
///
///     var numbers = [10, 12, -5, 14, -3, -9, 15]
///     let negativeSubranges = numbers.indices(where: { $0 < 0 })
///     // numbers[negativeSubranges].count == 3
///
///     numbers.moveSubranges(negativeSubranges, to: 0)
///     // numbers == [-5, -3, -9, 10, 12, 14, 15]
@available(SwiftStdlib 6.0, *)
public struct RangeSet<Bound: Comparable> {
  @usableFromInline
  internal var _ranges: Ranges

  /// A collection of the ranges that make up the range set.
  ///
  /// The ranges that you access by using `ranges` never overlap, are never
  /// empty, and are always in increasing order.
  public var ranges: Ranges {
    return _ranges
  }

  /// Creates an empty range set.
  public init() {
    _ranges = Ranges()
  }
  
  /// Creates a range set containing the given range.
  ///
  /// - Parameter range: The range to use for the new range set.
  public init(_ range: Range<Bound>) {
    if !range.isEmpty {
      self._ranges = Ranges(_range: range)
    } else {
      self._ranges = Ranges()
    }
  }
  
  /// Creates a range set containing the values in the given ranges.
  ///
  /// Any empty ranges in `ranges` are ignored, and non-empty ranges are merged
  /// to eliminate any overlaps. As such, the `ranges` collection in the
  /// resulting range set may not be equivalent to the sequence of ranges
  /// passed to this initializer.
  ///
  /// - Parameter ranges: The ranges to use for the new range set.
  public init(_ ranges: some Sequence<Range<Bound>>) {
    if let ranges = _specialize(ranges, for: Ranges.self) {
      self.init(_ranges: ranges)
      return
    }
    self.init(_ranges: Ranges(_unorderedRanges: Array(ranges)))
  }

  @usableFromInline
  internal init(_ranges: Ranges) {
    self._ranges = _ranges
  }
  
  /// Checks the invariants of `_ranges`.
  ///
  /// The ranges stored by a range set are never empty, never overlap,
  /// and are always stored in ascending order when comparing their lower
  /// or upper bounds. In addition to not overlapping, no two consecutive
  /// ranges share an upper and lower bound — `[0..<5, 5..<10]` is ill-formed,
  /// and would instead be represented as `[0..<10]`.
  @usableFromInline
  internal func _checkInvariants() {
#if INTERNAL_CHECKS_ENABLED
    for (a, b) in zip(_ranges, _ranges.dropFirst()) {
      _precondition(!a.isEmpty && !b.isEmpty, "Empty range in range set")
      _precondition(
        a.upperBound < b.lowerBound,
        "Out of order/overlapping ranges in range set")
    }
#endif
  }
  
  /// Creates a new range set from `ranges`, which satisfies the range set
  /// invariants.
  @usableFromInline
  internal init(_orderedRanges ranges: [Range<Bound>]) {
    switch ranges.count {
    case 0: self._ranges = Ranges()
    case 1: self._ranges = Ranges(_range: ranges[0])
    default: self._ranges = Ranges(_ranges: ranges)
    }
    _checkInvariants()
  }
  
  /// A Boolean value indicating whether the range set is empty.
  public var isEmpty: Bool {
    _ranges.isEmpty
  }
  
  /// Returns a Boolean value indicating whether the given value is
  /// contained by the ranges in the range set.
  ///
  /// - Parameter value: The value to look for in the range set.
  /// - Returns: `true` if `value` is contained by a range in the range set;
  ///   otherwise, `false`.
  ///
  /// - Complexity: O(log *n*), where *n* is the number of ranges in the
  ///   range set.
  public func contains(_ value: Bound) -> Bool {
    _ranges._contains(value)
  }
  
  /// Inserts the given range into the range set.
  ///
  /// - Parameter range: The range to insert into the set.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  @inlinable
  public mutating func insert(contentsOf range: Range<Bound>) {
    if range.isEmpty { return }
    _ranges._insert(contentsOf: range)
  }
  
  /// Removes the given range from the range set.
  ///
  /// - Parameter range: The range to remove from the set.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func remove(contentsOf range: Range<Bound>) {
    if range.isEmpty { return }
    _ranges._remove(contentsOf: range)
  }
}

@available(SwiftStdlib 6.0, *)
extension RangeSet: Equatable {
  public static func == (left: Self, right: Self) -> Bool {
    left._ranges == right._ranges
  }
}

@available(SwiftStdlib 6.0, *)
extension RangeSet: Hashable where Bound: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(self._ranges)
  }
}

@available(SwiftStdlib 6.0, *)
extension RangeSet: Sendable where Bound: Sendable {}

// MARK: - Collection APIs

@available(SwiftStdlib 6.0, *)
extension RangeSet {
  /// Creates a new range set containing ranges that contain only the
  /// specified indices in the given collection.
  ///
  /// - Parameters:
  ///   - index: The index to include in the range set. `index` must be a
  ///     valid index of `collection` that isn't the collection's `endIndex`.
  ///   - collection: The collection that contains `index`.
  @inlinable
  public init<S: Sequence, C: Collection>(
    _ indices: S, within collection: C
  ) where S.Element == C.Index, C.Index == Bound {
    self.init()
    for i in indices {
      self.insert(i, within: collection)
    }
  }

  /// Inserts a range that contains only the specified index into the range
  /// set.
  ///
  /// - Parameters:
  ///   - index: The index to insert into the range set. `index` must be a
  ///     valid index of `collection` that isn't the collection's `endIndex`.
  ///   - collection: The collection that contains `index`.
  ///
  /// - Returns: `true` if the range set was modified, or `false` if
  ///   the given `index` was already in the range set.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  @discardableResult
  public mutating func insert<C: Collection>(
    _ index: Bound, within collection: C
  ) -> Bool where C.Index == Bound {
    _ranges._insert(contentsOf: index ..< collection.index(after: index))
  }
  
  /// Removes the range that contains only the specified index from the range
  /// set.
  ///
  /// - Parameters:
  ///   - index: The index to remove from the range set. `index` must be a
  ///     valid index of `collection` that isn't the collection's `endIndex`.
  ///   - collection: The collection that contains `index`.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func remove<C: Collection>(
    _ index: Bound, within collection: C
  ) where C.Index == Bound {
    remove(contentsOf: index ..< collection.index(after: index))
  }

  /// Returns a range set that represents all the elements in the given
  /// collection that aren't represented by this range set.
  ///
  /// - Parameter collection: The collection that the range set is relative
  ///   to.
  /// - Returns: A new range set that represents the elements in `collection`
  ///   that aren't represented by this range set.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  @usableFromInline
  internal func _inverted<C: Collection>(
    within collection: C
  ) -> RangeSet where C: Collection, C.Index == Bound {
    return RangeSet(_ranges: _ranges._gaps(
      boundedBy: collection.startIndex ..< collection.endIndex))
  }
}

// MARK: - SetAlgebra

// These methods only depend on the ranges that comprise the range set, so
// we can provide them even when we can't provide `SetAlgebra` conformance.
@available(SwiftStdlib 6.0, *)
extension RangeSet {
  /// Adds the contents of the given range set to this range set.
  ///
  /// - Parameter other: A range set to merge with this one.
  ///
  /// - Complexity: O(*m* + *n*), where *m* and *n* are the number of ranges in
  ///   this and the other range set.
  public mutating func formUnion(_ other: __owned RangeSet<Bound>) {
    self = self.union(other)
  }
  
  /// Removes the contents of this range set that aren't also in the given
  /// range set.
  ///
  /// - Parameter other: A range set to intersect with.
  public mutating func formIntersection(_ other: RangeSet<Bound>) {
    self = self.intersection(other)
  }
  
  /// Removes the contents of this range set that are also in the given set
  /// and adds the contents of the given set that are not already in this
  /// range set.
  ///
  /// - Parameter other: A range set to perform a symmetric difference against.
  public mutating func formSymmetricDifference(
    _ other: __owned RangeSet<Bound>
  ) {
    self = self.symmetricDifference(other)
  }
  
  /// Removes the contents of the given range set from this range set.
  ///
  /// - Parameter other: A range set to subtract from this one.
  public mutating func subtract(_ other: RangeSet<Bound>) {
    for range in other._ranges {
      remove(contentsOf: range)
    }
  }
  
  /// Returns a new range set containing the contents of both this set and the
  /// given set.
  ///
  /// - Parameter other: The range set to merge with this one.
  /// - Returns: A new range set.
  public __consuming func union(
    _ other: __owned RangeSet<Bound>
  ) -> RangeSet<Bound> {
    return RangeSet(_ranges: _ranges._union(other._ranges))
  }
  
  /// Returns a new range set containing the contents of both this set and the
  /// given set.
  ///
  /// - Parameter other: The range set to merge with this one.
  /// - Returns: A new range set.
  public __consuming func intersection(
    _ other: RangeSet<Bound>
  ) -> RangeSet<Bound> {
    return RangeSet(_ranges: _ranges._intersection(other._ranges))
  }
  
  /// Returns a new range set representing the values in this range set or the
  /// given range set, but not both.
  ///
  /// - Parameter other: The range set to find a symmetric difference with.
  /// - Returns: A new range set.
  public __consuming func symmetricDifference(
    _ other: __owned RangeSet<Bound>
  ) -> RangeSet<Bound> {
    return union(other).subtracting(intersection(other))
  }
  
  /// Returns a new set containing the contents of this range set that are not
  /// also in the given range set.
  ///
  /// - Parameter other: The range set to subtract.
  /// - Returns: A new range set.
  public __consuming func subtracting(
    _ other: RangeSet<Bound>
  ) -> RangeSet<Bound> {
    var result = self
    result.subtract(other)
    return result
  }
  
  /// Returns a Boolean value that indicates whether this range set is a
  /// subset of the given set.
  ///
  /// - Parameter other: A range set to compare against.
  /// - Returns: `true` if this range set is a subset of `other`;
  ///   otherwise, `false`.
  public func isSubset(of other: RangeSet<Bound>) -> Bool {
    var remaining = other.ranges[...]
    for range in ranges {
      guard let containingIdx = remaining.firstIndex(where: {
        $0.contains(range.lowerBound)
      }) else {
        return false
      }

      if remaining[containingIdx].upperBound < range.upperBound {
        return false
      }

      remaining = other.ranges[containingIdx...]
    }
    return true
  }
  
  /// Returns a Boolean value that indicates whether this range set is a
  /// superset of the given set.
  ///
  /// - Parameter other: A range set to compare against.
  /// - Returns: `true` if this range set is a superset of `other`;
  ///   otherwise, `false`.
  public func isSuperset(of other: RangeSet<Bound>) -> Bool {
    other.isSubset(of: self)
  }
  
  /// Returns a Boolean value that indicates whether this range set is a
  /// strict subset of the given set.
  ///
  /// - Parameter other: A range set to compare against.
  /// - Returns: `true` if this range set is a strict subset of `other`;
  ///   otherwise, `false`.
  public func isStrictSubset(of other: RangeSet<Bound>) -> Bool {
    self != other && isSubset(of: other)
  }
  
  /// Returns a Boolean value that indicates whether this range set is a
  /// strict superset of the given set.
  ///
  /// - Parameter other: A range set to compare against.
  /// - Returns: `true` if this range set is a strict superset of `other`;
  ///   otherwise, `false`.
  public func isStrictSuperset(of other: RangeSet<Bound>) -> Bool {
    other.isStrictSubset(of: self)
  }

  /// Returns a Boolean value that indicates whether this range set set has
  /// no members in common with the given set.
  ///
  /// - Parameter other: A range set to compare against.
  /// - Returns: `true` if this range set has no elements in common with
  ///   `other`; otherwise, `false`.
  public func isDisjoint(_ other: RangeSet<Bound>) -> Bool {
    self.intersection(other).isEmpty
  }
}

@available(SwiftStdlib 6.0, *)
@_unavailableInEmbedded
extension RangeSet: CustomStringConvertible {
  public var description: String {
    return _ranges.description
  }
}
