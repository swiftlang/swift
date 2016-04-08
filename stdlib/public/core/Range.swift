//===--- Range.swift ------------------------------------------------------===//
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

// A dummy type that we can use when we /don't/ want to create an
// ambiguity indexing CountableRange<T> outside a generic context.
public enum _DisabledRangeIndex_ {}

/// A type that represents a contiguous range of any comparable value.
///
/// A range contains at least every value `x` where `lowerBound < x` and
/// `x < upperBound`. Individual types that conform to `RangeProtocol` must
/// specify their containment rules for the bounds of the range.
///
/// The standard library defines two kinds of ranges: closed ranges,
/// represented by `ClosedRangeProtocol`, and half-open ranges, represented by
/// `HalfOpenRangeProtocol`. A closed range contains both its lower and upper
/// bounds, and therefore cannot be empty. A half-open range, on the other
/// hand, contains its lower bound when nonempty but never its upper bound.
///
///     let closed: ClosedRange = 5...10
///     closed.contains(5)      // true
///     closed.contains(10)     // true
///
///     let halfOpen: Range = 5..<10
///     halfOpen.contains(5)    // true
///     halfOpen.contains(10)   // false
///
/// Not all empty ranges are equal; the bounds of two empty ranges must also be
/// equal for the ranges to be equal.
public protocol RangeProtocol : Equatable {
  
  /// The representation of the range's endpoints and the values
  /// contained in it.
  associatedtype Bound : Comparable

  /// Creates an instance with the given bounds.
  ///
  /// - Note: As this initializer does not check its precondition, it
  ///   should be used as an optimization only, when one is absolutely
  ///   certain that `lower <= upper`.  In general, the `..<` and `...`
  ///   operators are to be preferred for forming ranges.
  ///
  /// - Precondition: `lower <= upper`
  init(uncheckedBounds: (lower: Bound, upper: Bound))

  /// Returns `true` if the range contains the `value`.
  ///
  /// Any type of range contains every value `x` where
  /// `lowerBound < x < upperBound`. `RangeProtocol` makes no requirement as
  /// to whether individual range types must contain either their lower or
  /// upper bound.
  func contains(value: Bound) -> Bool
  
  /// Returns `true` iff `self` and `other` contain a value in common.
  /// 
  /// Any type of range contains every value `x` where
  /// `lowerBound < x < upperBound`. `RangeProtocol` makes no requirement as
  /// to whether individual range types must contain either their lower or
  /// upper bound.
  func overlaps(other: Self) -> Bool

  /// Returns `true` iff `self.contains(x)` is `false` for all values of `x`.
  var isEmpty: Bool { get }
  
  // Note: When the range is also a collection, it is crucial to
  // enforce the invariant that lowerBound <= upperBound, or it may be
  // empty with startIndex != endIndex.

  /// The range's lower bound.
  ///
  /// Depending on the concrete type of the range, `lowerBound` may or may not
  /// be contained in the range.
  var lowerBound: Bound { get }
  
  /// The range's upper bound.
  ///
  /// Depending on the concrete type of the range, `upperBound` may or may not
  /// be contained in the range.
  var upperBound: Bound { get }
  
  /// Returns `self` clamped to `limits`.
  ///
  /// The bounds of the result, even if it is empty, are always
  /// limited to the bounds of `limits`.
  func clamped(to limits: Self) -> Self
}

extension RangeProtocol {
  /// Creates a copy of `other`.
  @inline(__always)
  public init(_ other: Self) {
    self = other
  }
  
  /// Returns `true` iff `self` and `other` contain a value in common.
  @inline(__always)
  public func overlaps<
    Other: RangeProtocol where Other.Bound == Bound
  >(other: Other) -> Bool {
    return (!other.isEmpty && self.contains(other.lowerBound))
        || (!self.isEmpty && other.contains(lowerBound))
  }
  
  /// Returns `true` iff `self.contains(x)` is `false` for all values of `x`.
  public var isEmpty: Bool {
    // Note: this default implementation is not suitable for a range
    // that has an open lower bound.
    return !self.contains(self.lowerBound)
  }
  
  /// Returns `self` clamped to `limits`.
  ///
  /// The bounds of the result, even if it is empty, are always
  /// limited to the bounds of `limits`.
  @inline(__always)
  public func clamped(to limits: Self) -> Self {
    return Self(
      uncheckedBounds: (
        lower:
        limits.lowerBound > self.lowerBound ? limits.lowerBound
          : limits.upperBound < self.lowerBound ? limits.upperBound
          : self.lowerBound,
        upper:
          limits.upperBound < self.upperBound ? limits.upperBound
          : limits.lowerBound > self.upperBound ? limits.lowerBound
          : self.upperBound
      )
    )
  }
}

/// A type that represents a contiguous range of any `Comparable` value,
/// with nonempty ranges containing their lower bounds but not their
/// upper bounds.
/// 
/// A half-open range can represent an empty range.
public protocol HalfOpenRangeProtocol : RangeProtocol {}

@warn_unused_result
public func == <
  LHS : HalfOpenRangeProtocol, RHS : HalfOpenRangeProtocol 
  where LHS.Bound == RHS.Bound
>(lhs: LHS, rhs: RHS) -> Bool {
  return
    lhs.lowerBound == rhs.lowerBound &&
    lhs.upperBound == rhs.upperBound
}


extension HalfOpenRangeProtocol {
  /// Creates an instance equivalent to `other`.
  @inline(__always)
  public init<
    Other: HalfOpenRangeProtocol where Other.Bound == Bound
  >(_ other: Other) {
    self.init(
      uncheckedBounds: (lower: other.lowerBound, upper: other.upperBound)
    )
  }
  
  /// Returns `true` iff `lowerBound <= value && upperBound > value`.
  @inline(__always)
  public func contains(value: Bound) -> Bool {
    return lowerBound <= value && upperBound > value
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(lowerBound)..<\(upperBound)"
  }
}

/// Half-open ranges whose `Bound` is `Strideable` with `Integer`
/// `Stride` have all the capabilities of `RandomAccessCollection`s,
/// where elements of the collection are the values contained in the
/// range.
extension HalfOpenRangeProtocol
  where Bound : _Strideable, Bound.Stride : Integer {
  // WORKAROUND rdar://25214598 - should be Bound : Strideable

  /// The number of values contained in the range. 
  public var count: Bound.Stride {
    return lowerBound.distance(to: upperBound)
  }
  
  /// Accesses the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != upperBound`.
  public subscript(position: Bound) -> Bound {
    _debugPrecondition(self.contains(position), "Index out of range")
    return position
  }

  public subscript(bounds: Range<Bound>) -> Self {
    return Self(bounds)
  }

  public subscript(bounds: Self) -> Self {
    return bounds
  }

  public var startIndex: Bound {
    return lowerBound
  }
  
  public var endIndex: Bound {
    return upperBound
  }

  public var indices: Self {
    return self
  }
}

/// A half-open range that forms a collection of consecutive `Strideable`
/// values.
///
/// A `CountableRange` instance contains its `lowerBound` but not its upper
/// bound. Like other collections, a `CountableRange` containing one element
/// has an `upperBound` that is the successor of its `lowerBound` and an empty
/// `CountableRange` has `lowerBound == upperBound`.
///
/// The associated `Bound` type is both the element type and the index type of
/// `CountableRange`. Each element of the range is its own corresponding
/// index, so for any countable range `r`, `r[i] == i`.
///
/// Therefore, if `Bound` has a maximal value, it can serve as an `upperBound`,
/// but can never be contained in a `CountableRange<Bound>`.
///
///     let maximumRange = Int8.min..<Int8.max
///     maximumRange.contains(Int8.max)   // false
///
/// It also follows from the requirement above that `(-99..<100)[0] == 0`. To
/// prevent confusion (because some might expect the result to be `-99`), in a
/// context where `Bound` is known to be an integer type, subscripting with
/// `Bound` is a compile-time error:
///
///     // error: ambiguous use of 'subscript'
///     print(CountableRange<Int>(uncheckedBounds: (-99, 100))[0])
///
/// However, subscripting that range still works in a generic context:
///
///     func brackets<T>(x: CountableRange<T>, _ i: T) -> T {
///         return x[i] // Just forward to subscript
///     }
///     print(brackets(CountableRange<Int>(uncheckedBounds: (-99, 100)), 0))
///     // Prints "0"
///     
/// - SeeAlso: `CountableClosedRange`, `Range`, `ClosedRange`
public struct CountableRange<
  // WORKAROUND rdar://25214598 - should be just Bound : Strideable
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> : Equatable, RandomAccessCollection,
  CustomStringConvertible, CustomDebugStringConvertible, 
  HalfOpenRangeProtocol {

  public typealias Element = Bound
  public typealias Index = Element
  
  // WORKAROUND - default should be picked up from Collection
  @warn_unused_result
  public func successor(of i: Index) -> Index {
    // FIXME: swift-3-indexing-model: tests.
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)

    return i.advanced(by: 1)
  }

  // WORKAROUND - default should be picked up from Collection
  @warn_unused_result
  public func predecessor(of i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check i: should allow `endIndex`.
    //_failEarlyRangeCheck(i, bounds: startIndex..<endIndex)

    return i.advanced(by: -1)
  }

  /// Creates an instance with the given bounds.
  ///
  /// - Note: As this initializer does not check its precondition, it should be
  ///   used as an optimization only, when one is absolutely certain that
  ///   `lower <= upper`. Using the `..<` operator to form `CountableRange`
  ///   instances is preferred.
  /// - Precondition: `lower <= upper`
  public init(uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }
  
  // FIXME(compiler limitation): this typealias should be inferred.
  public typealias SubSequence = CountableRange<Bound>

  public subscript(bounds: Range<Element>) -> CountableRange<Bound> {
    return CountableRange(bounds)
  }

  public subscript(
    bounds: CountableRange<Bound>
  ) -> CountableRange<Bound> {
    return self[Range(bounds)]
  }
  
  /// The range's lower bound.
  ///
  /// Identical to `upperBound` in an empty range.
  public let lowerBound: Bound

  /// The range's upper bound.
  ///
  /// `upperBound` is not a valid argument to `subscript`, and is always
  /// reachable from `lowerBound` by zero or more applications of
  /// `successor(of:)`.
  /// 
  /// Identical to `lowerBound` in an empty range.
  public let upperBound: Bound
  
  // FIXME(compiler limitation): this typealias should be inferred.
  public typealias Indices = CountableRange<Bound>

  @warn_unused_result
  public func _customContainsEquatableElement(element: Element) -> Bool? {
    return element >= self.lowerBound && element < self.upperBound
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "CountableRange(\(String(reflecting: lowerBound))..<\(String(reflecting: upperBound)))"
  }

  public // ambiguity resolution between RangeProtocol and Collection defaults
  var isEmpty: Bool {
    return lowerBound == upperBound
  }
}

//===--- Protection against 0-based indexing assumption -------------------===//
// The following two extensions provide subscript overloads that
// create *intentional* ambiguities to prevent the use of integers as
// indices for ranges, outside a generic context.  This prevents mistakes
// such as x = r[0], which will trap unless 0 happens to be contained in the
// range r.
extension CountableRange {
  /// Accesses the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != upperBound`.
  public subscript(position: Bound) -> Bound {
    _debugPrecondition(self.contains(position), "Index out of range")
    return position
  }

  public subscript(_position: Bound._DisabledRangeIndex) -> Bound {
    fatalError("uncallable")
  }
}

extension CountableRange 
  where
Bound._DisabledRangeIndex : Strideable, 
Bound._DisabledRangeIndex.Stride : Integer {
  
  public subscript(
    _bounds: Range<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: CountableRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }
  
  public subscript(
    _bounds: ClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: CountableClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  /// Accesses the subsequence bounded by `bounds`.
  ///
  /// - Complexity: O(1)
  /// - Precondition: `(startIndex...endIndex).contains(bounds.lowerBound)`
  ///   and `(startIndex...endIndex).contains(bounds.upperBound)`
  public subscript(bounds: ClosedRange<Bound>) -> CountableRange<Bound> {
    return self[bounds.lowerBound..<(bounds.upperBound.advanced(by: 1))]
  }

  /// Accesses the subsequence bounded by `bounds`.
  ///
  /// - Complexity: O(1)
  /// - Precondition: `(startIndex...endIndex).contains(bounds.lowerBound)`
  ///   and `(startIndex...endIndex).contains(bounds.upperBound)`
  public subscript(
    bounds: CountableClosedRange<Bound>
  ) -> CountableRange<Bound> {
    return self[ClosedRange(bounds)]
  }
}

//===--- End 0-based indexing protection ----------------------------------===//

extension CountableRange : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["lowerBound": lowerBound, "upperBound": upperBound])
  }
}

/// An interval over a `Comparable` type, from a lower bound up to, but not
/// including, an upper bound. Can represent an empty interval.
///
/// Use a `Range` to quickly check if a `Comparable` value is contained in a
/// particular range of values. For example:
///
///     let underFive: Range = 0.0..<5.0
///     underFive.contains(3.14)    // true
///     underFive.contains(6.28)    // false
///     underFive.contains(5.0)     // false
public struct Range<
  Bound : Comparable
> : Equatable, CustomStringConvertible, CustomDebugStringConvertible,
    HalfOpenRangeProtocol {

  /// Creates a range with `lowerBound == lower` and `upperBound == upper`.
  ///
  /// - Note: As this initializer does not check its precondition, it should be
  ///   used as an optimization only, when one is absolutely certain that
  ///   `lower <= upper`. Using the `..<` operator to form `Range` instances
  ///   is preferred.
  /// - Precondition: `lower <= upper`
  @inline(__always)
  public init(uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }

  /// The range's lower bound.
  ///
  /// Identical to `upperBound` in an empty range.
  public let lowerBound: Bound

  /// The range's upper bound.
  ///
  /// Identical to `lowerBound` in an empty range. A `Range` instance
  /// does not contain its `upperBound`.
  public let upperBound: Bound

  /// Returns `true` iff `element` is between `lowerBound` and `upperBound` or
  /// equal to `lowerBound`. A `Range` instance does not contain its
  /// `upperBound`.
  @warn_unused_result
  public func contains(element: Bound) -> Bool {
    return element >= self.lowerBound && element < self.upperBound
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(lowerBound)..<\(upperBound)"
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "Range(\(String(reflecting: lowerBound))..<\(String(reflecting: upperBound)))"
  }
}

extension Range : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["lowerBound": lowerBound, "upperBound": upperBound])
  }
}

//===--- Protection against 0-based indexing assumption -------------------===//
// The following two extensions provide subscript overloads that
// create *intentional* ambiguities to prevent the use of integers as
// indices for ranges, outside a generic context.  This prevents mistakes
// such as x = r[0], which will trap unless 0 happens to be contained in the
// range r.
extension Range where Bound : _Strideable, Bound.Stride : Integer {
  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != upperBound`.
  public subscript(position: Bound) -> Bound {
    _debugPrecondition(self.contains(position), "Index out of range")
    return position
  }

  public subscript(_position: Bound._DisabledRangeIndex) -> Bound {
    fatalError("uncallable")
  }
}

extension Range
  where 
Bound : Strideable, 
Bound.Stride : Integer, 
Bound._DisabledRangeIndex : Strideable,
Bound._DisabledRangeIndex.Stride : Integer {
  public subscript(_bounds: Range<Bound>) -> Range {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: Range<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: CountableRange<Bound>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: CountableRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(_bounds: ClosedRange<Bound>) -> Range {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: ClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: CountableClosedRange<Bound>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    _bounds: CountableClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }
}
//===--- End 0-based indexing protection ----------------------------------===//

/// Returns a half-open range that contains `minimum`, but not
/// `maximum`.
@_transparent
@warn_unused_result
public func ..< <Bound : Comparable> (minimum: Bound, maximum: Bound)
  -> Range<Bound> {
  _precondition(minimum <= maximum,
    "Can't form Range with upperBound < lowerBound")
  return Range(uncheckedBounds: (lower: minimum, upper: maximum))
}

/// Returns a half-open range that contains `minimum`, but not `maximum`.
///
/// - Precondition: `minimum <= maximum`.
@_transparent
@warn_unused_result
// WORKAROUND rdar://25214598 - should be just Bound : Strideable
public func ..< <
  Bound : _Strideable where Bound : Comparable, Bound.Stride : Integer
> (
  minimum: Bound, maximum: Bound
) -> CountableRange<Bound> {
  // FIXME: swift-3-indexing-model: tests for traps.
  _precondition(minimum <= maximum,
    "Can't form Range with upperBound < lowerBound")
  return CountableRange(uncheckedBounds: (lower: minimum, upper: maximum))
}

@warn_unused_result
public func ~= <R: RangeProtocol> (
  pattern: R, value: R.Bound
) -> Bool {
  return pattern.contains(value)
}

// swift-3-indexing-model: this is not really a proper rename
@available(*, unavailable, renamed: "IndexingIterator")
public struct RangeGenerator<Bound> {}
