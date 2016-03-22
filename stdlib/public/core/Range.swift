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
// ambiguity indexing RangeOfStrideable<T> outside a generic context.
public enum _DisabledRangeIndex_ {}

// FIXME swift-3-indexing-model: Decide whether to de-underscore these
// protocols and/or to incorporate their default implementations into
// the concrete models.

public // implementation sharing
protocol _RangeProtocol {
  associatedtype Bound : Comparable
  
  init(_uncheckedBounds: (lower: Bound, upper: Bound))
  func contains(value: Bound) -> Bool
  func overlaps(other: Self) -> Bool
  var isEmpty: Bool { get }
  
  // Note: it is crucial to enforce the invariant that lowerBound <=
  // upperBound, or when Range has a Stridable Bound, and is thus a
  // collection, it may be empty with startIndex != endIndex.  Thus,
  // make sure lowerBound and upperBound are not settable in concrete models.
  var lowerBound: Bound { get }
  var upperBound: Bound { get }
  
  /// Returns `self` clamped to `limits`.
  ///
  /// The bounds of the result, even if it is empty, are always
  /// limited to the bounds of `limits`.
  func clamped(to limits: Self) -> Self
}

extension _RangeProtocol {

  @inline(__always)
  public init(_ other: Self) {
    self = other
  }
  
  @inline(__always)
  public func overlaps<
    Other: _RangeProtocol where Other.Bound == Bound
  >(other: Other) -> Bool {
    return (!other.isEmpty && self.contains(other.lowerBound))
        || (!self.isEmpty && other.contains(lowerBound))
  }
  
  public var isEmpty: Bool {
    return !self.contains(self.lowerBound)
  }
  
  @inline(__always)
  public func clamped(to limits: Self) -> Self {
    return Self(
      _uncheckedBounds: (
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

public // implementation sharing
protocol _HalfOpenRange : _RangeProtocol {}

extension _HalfOpenRange {
  @inline(__always)
  public init<
    Other: _HalfOpenRange where Other.Bound == Bound
  >(_ other: Other) {
    self.init(
      _uncheckedBounds: (lower: other.lowerBound, upper: other.upperBound)
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

// WORKAROUND rdar://25214598 - should be Bound : Strideable
extension _HalfOpenRange where Bound : _Strideable {
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

public struct RangeOfStrideable<
  // WORKAROUND rdar://25214598 - should be just Bound : Strideable
  Bound : Comparable where Bound : _Strideable
> : Equatable, RandomAccessCollection,
  CustomStringConvertible, CustomDebugStringConvertible, 
  _HalfOpenRange {

  public typealias Element = Bound
  public typealias Index = Element
  
  // WORKAROUND - default should be picked up from Collection
  @warn_unused_result
  public func next(i: Index) -> Index {
    // FIXME: swift-3-indexing-model: tests.
    _failEarlyRangeCheck(i, bounds: startIndex..<endIndex)

    return i.advanced(by: 1)
  }

  // WORKAROUND - default should be picked up from Collection
  @warn_unused_result
  public func previous(i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range check i: should allow `endIndex`.
    //_failEarlyRangeCheck(i, bounds: startIndex..<endIndex)

    return i.advanced(by: -1)
  }

  public init(_uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }
  
  // FIXME(compiler limitation): this typealias should be inferred.
  public typealias SubSequence = RangeOfStrideable<Bound>

  public subscript(bounds: Range<Element>) -> RangeOfStrideable<Bound> {
    return RangeOfStrideable(bounds)
  }

  public subscript(
    bounds: RangeOfStrideable<Bound>
  ) -> RangeOfStrideable<Bound> {
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
  public typealias Indices = RangeOfStrideable<Bound>

  @warn_unused_result
  public func _customContainsEquatableElement(element: Element) -> Bool? {
    return element >= self.lowerBound && element < self.upperBound
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "RangeOfStrideable(\(String(reflecting: lowerBound))..<\(String(reflecting: upperBound)))"
  }

  public // ambiguity resolution between _RangeProtocol and Collection defaults
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
extension RangeOfStrideable {
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

extension RangeOfStrideable where Bound._DisabledRangeIndex : Strideable {
  public subscript(bounds: Range<Bound._DisabledRangeIndex>) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: RangeOfStrideable<Bound._DisabledRangeIndex>
  ) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }
  
  public subscript(bounds: ClosedRange<Bound._DisabledRangeIndex>) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: ClosedRangeOfStrideable<Bound._DisabledRangeIndex>
  ) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(bounds: ClosedRange<Bound>) -> RangeOfStrideable<Bound> {
    return self[bounds.lowerBound..<(bounds.upperBound.advanced(by: 1))]
  }

  public subscript(
    bounds: ClosedRangeOfStrideable<Bound>
  ) -> RangeOfStrideable<Bound> {
    return self[ClosedRange(bounds)]
  }
}

//===--- End 0-based indexing protection ----------------------------------===//

extension RangeOfStrideable : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["lowerBound": lowerBound, "upperBound": upperBound])
  }
}

@warn_unused_result
public func == <Bound>(
  lhs: RangeOfStrideable<Bound>,
  rhs: RangeOfStrideable<Bound>
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
    _HalfOpenRange {

  /// Construct a range with `lowerBound == start` and `upperBound ==
  /// end`.
  @inline(__always)
  public init(_uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
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
extension Range where Bound : _Strideable {
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

extension Range where Bound : Strideable, Bound._DisabledRangeIndex : Strideable {
  public subscript(bounds: Range<Bound>) -> Range {
    fatalError("uncallable")
  }

  public subscript(
    bounds: Range<Bound._DisabledRangeIndex>
  ) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: RangeOfStrideable<Bound>
  ) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: RangeOfStrideable<Bound._DisabledRangeIndex>
  ) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(bounds: ClosedRange<Bound>) -> Range {
    fatalError("uncallable")
  }

  public subscript(
    bounds: ClosedRange<Bound._DisabledRangeIndex>
  ) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: ClosedRangeOfStrideable<Bound>
  ) -> RangeOfStrideable<Bound> {
    fatalError("uncallable")
  }

  public subscript(
    bounds: ClosedRangeOfStrideable<Bound._DisabledRangeIndex>
  ) -> RangeOfStrideable<Bound> {
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
  return Range(_uncheckedBounds: (lower: minimum, upper: maximum))
}

/// Returns a half-open range that contains `start`, but not `end`.
///
/// - Precondition: `start <= end`.
@_transparent
@warn_unused_result
// WORKAROUND rdar://25214598 - should be just Bound : Strideable
public func ..< <Bound : _Strideable where Bound : Comparable> (
  start: Bound, end: Bound
) -> RangeOfStrideable<Bound> {
  // FIXME: swift-3-indexing-model: tests for traps.
  _precondition(start <= end, "Can't form Range with end < start")
  return RangeOfStrideable(_uncheckedBounds: (lower: start, upper: end))
}

@warn_unused_result
public func ~= <R: _RangeProtocol> (
  pattern: R, value: R.Bound
) -> Bool {
  return pattern.contains(value)
}

// swift-3-indexing-model: this is not really a proper rename
@available(*, unavailable, renamed: "IndexingIterator")
public struct RangeGenerator<Bound> {}
