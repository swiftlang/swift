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
/// - Note: not all empty ranges are equal; it depends on their bounds.
///
/// - IXMEFAY: need a more formal description of containment and its
///   relationship to bounds.  We *can* say that any x > lowerBound
///   and < upperBound is contained, and containment of the bounds
///   themselves are up to the model.
public protocol RangeProtocol : Equatable {
  
  /// The representation of the range's endpoints and the values
  /// contained in it.
  associatedtype Bound : Comparable

  /// Creates an instance with the given bounds.
  ///
  /// - Note: as this initializer does not check its precondition, it
  ///   should be used as an optimization only, when one is absolutely
  ///   certain that `lower <= upper`.  In general, the `..<` and `...`
  ///   operators are to be preferred for forming ranges.
  ///
  /// - Precondition: `lower <= upper`
  init(uncheckedBounds: (lower: Bound, upper: Bound))

  /// Returns `true` if the range contains the `value`.
  ///
  /// - IXMEFAY: need a caveat that this protocol doesn't fully define
  ///   containment and its relationship to bounds.  
  func contains(value: Bound) -> Bool
  
  /// Returns `true` iff `self` and `other` contain a value in common.
  func overlaps(other: Self) -> Bool

  /// Returns `true` iff `self.contains(x)` is `false` for all values of `x`.
  var isEmpty: Bool { get }
  
  // Note: When the range is also a collection, it is crucial to
  // enforce the invariant that lowerBound <= upperBound, or it may be
  // empty with startIndex != endIndex.

  /// The range's lower bound.
  ///
  /// - IXMEFAY: vacuous comment
  var lowerBound: Bound { get }
  
  /// The range's upper bound.
  ///
  /// - IXMEFAY: vacuous comment
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

/// A type that represents a contiguous range of any comparable value,
/// with non-empty ranges containing their lower bounds but not their
/// upper bounds.
public protocol HalfOpenRangeProtocol : RangeProtocol {}

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
  
  @inline(__always)
  public func contains(value: Bound) -> Bool {
    return lowerBound <= value && upperBound > value
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(lowerBound)..<\(upperBound)"
  }
}

/// Ranges whose `Bound` is `Strideable` with `Integer` `Stride`
/// have all the capabilities of `RandomAccessCollection`s, where
/// elements of the collection are the values contained in the range.
extension HalfOpenRangeProtocol
  where Bound : _Strideable, Bound.Stride : Integer {
  // WORKAROUND rdar://25214598 - should be Bound : Strideable

  /// The number of values contained in the range. 
  public var count: Bound.Stride {
    return lowerBound.distance(to: upperBound)
  }
  
  /// Access the element at `position`.
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

/// A range whose `Bound` is `Strideable` with `Integer` `Stride`, and
/// that conforms to `RandomAccessCollection`.
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
  /// `successor()`.
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
  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != upperBound`.
  public subscript(position: Bound) -> Bound {
    _debugPrecondition(self.contains(position), "Index out of range")
    return position
  }

  public subscript(_: Bound._DisabledRangeIndex) -> Bound {
    fatalError("uncallable")
  }
}

extension CountableRange 
  where
Bound._DisabledRangeIndex : Strideable, 
Bound._DisabledRangeIndex.Stride : Integer {
  
  public subscript(
    bounds: Range<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: CountableRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }
  
  public subscript(
    bounds: ClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: CountableClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(bounds: ClosedRange<Bound>) -> CountableRange<Bound> {
    return self[bounds.lowerBound..<(bounds.upperBound.advanced(by: 1))]
  }

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

@warn_unused_result
public func == <Bound>(
  lhs: CountableRange<Bound>,
  rhs: CountableRange<Bound>
) -> Bool {
  return
    lhs.lowerBound == rhs.lowerBound &&
    lhs.upperBound == rhs.upperBound
}

/// A collection of consecutive discrete index values.
///
/// - parameter Element: Is both the element type and the index type of the
///   collection.
///
/// Like other collections, a range containing one element has an
/// `upperBound` that is the successor of its `lowerBound`; and an empty
/// range has `lowerBound == upperBound`.
///
/// Axiom: for any `Range` `r`, `r[i] == i`.
///
/// Therefore, if `Element` has a maximal value, it can serve as an
/// `upperBound`, but can never be contained in a `Range<Element>`.
///
/// It also follows from the axiom above that `(-99..<100)[0] == 0`.
/// To prevent confusion (because some expect the result to be `-99`),
/// in a context where `Element` is known to be an integer type,
/// subscripting with `Element` is a compile-time error:
///
///     // error: could not find an overload for 'subscript'...
///     print(Range<Int>(start: -99, end: 100)[0])
///
/// However, subscripting that range still works in a generic context:
///
///     func brackets<Element : ForwardIndex>(x: Range<Element>, i: Element) -> Element {
///       return x[i] // Just forward to subscript
///     }
///     print(brackets(Range<Int>(start: -99, end: 100), 0))
///     // Prints "0"
public struct Range<
  Bound : Comparable
> : Equatable, CustomStringConvertible, CustomDebugStringConvertible,
    HalfOpenRangeProtocol {

  /// Construct a range with `lowerBound == start` and `upperBound ==
  /// end`.
  @inline(__always)
  public init(uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }

  /// The range's lower bound.
  ///
  /// Identical to `upperBound` in an empty range.
  // FIXME: swift-3-indexing-model: rename to `start`.
  public let lowerBound: Bound

  /// The range's upper bound.
  ///
  /// `upperBound` is not a valid argument to `subscript`, and is always
  /// reachable from `lowerBound` by zero or more applications of
  /// `successor()`.
  // FIXME: swift-3-indexing-model: rename to `end`.
  public let upperBound: Bound

  // FIXME: does not implement a requirement in `Collection`.
  // We need to implement `_customContainsEquatableElement` instead.
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

  public subscript(_: Bound._DisabledRangeIndex) -> Bound {
    fatalError("uncallable")
  }
}

extension Range
  where 
Bound : Strideable, 
Bound.Stride : Integer, 
Bound._DisabledRangeIndex : Strideable,
Bound._DisabledRangeIndex.Stride : Integer {
  public subscript(bounds: Range<Bound>) -> Range {
    fatalError("uncallable")
  }

  public subscript(
    bounds: Range<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: CountableRange<Bound>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: CountableRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(bounds: ClosedRange<Bound>) -> Range {
    fatalError("uncallable")
  }

  public subscript(
    bounds: ClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: CountableClosedRange<Bound>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: CountableClosedRange<Bound._DisabledRangeIndex>
  ) -> CountableRange<Bound> {
    fatalError("uncallable")
  }
}
//===--- End 0-based indexing protection ----------------------------------===//

@warn_unused_result
public func == <Bound>(lhs: Range<Bound>, rhs: Range<Bound>) -> Bool {
  return
    lhs.lowerBound == rhs.lowerBound &&
    lhs.upperBound == rhs.upperBound
}

/// Returns a half-open range that contains `minimum`, but not
/// `maximum`.
@_transparent
@warn_unused_result
public func ..< <Bound : Comparable> (minimum: Bound, maximum: Bound)
  -> Range<Bound> {
  _precondition(minimum <= maximum, "Can't form Range with end < start")
  return Range(uncheckedBounds: (lower: minimum, upper: maximum))
}

/// Returns a half-open range that contains `start`, but not `end`.
///
/// - Precondition: `start <= end`.
@_transparent
@warn_unused_result
// WORKAROUND rdar://25214598 - should be just Bound : Strideable
public func ..< <
  Bound : _Strideable where Bound : Comparable, Bound.Stride : Integer
> (
  start: Bound, end: Bound
) -> CountableRange<Bound> {
  // FIXME: swift-3-indexing-model: tests for traps.
  _precondition(start <= end, "Can't form Range with end < start")
  return CountableRange(uncheckedBounds: (lower: start, upper: end))
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
