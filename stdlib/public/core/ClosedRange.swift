//===--- ClosedRange.swift ------------------------------------------------===//
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

public // implementation sharing
protocol _ClosedRange : _RangeProtocol {}

extension _ClosedRange {
  @inline(__always)
  public init<
    Other: _ClosedRange where Other.Bound == Bound
  >(_ other: Other) {
    self.init(
      _uncheckedBounds: (lower: other.lowerBound, upper: other.upperBound)
    )
  }
  
  @inline(__always)
  public func contains(value: Bound) -> Bool {
    return lowerBound <= value && upperBound >= value
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(lowerBound)...\(upperBound)"
  }
}

internal enum _ClosedRangeIndex<Bound: Strideable> {
case pastEnd
case inRange(Bound)
}

public struct ClosedRangeIndex<Bound: Strideable> : Comparable {
  internal var value : _ClosedRangeIndex<Bound>
}

public func == <B>(lhs: ClosedRangeIndex<B>, rhs: ClosedRangeIndex<B>) -> Bool {
  switch (lhs.value, rhs.value) {
  case (.inRange(let l), .inRange(let r)):
    return l == r
  case (.pastEnd, .pastEnd):
    return true
  default:
    return false
  }
}

public func < <B>(lhs: ClosedRangeIndex<B>, rhs: ClosedRangeIndex<B>) -> Bool {
  switch (lhs.value, rhs.value) {
  case (.inRange(let l), .inRange(let r)):
    return l < r
  case (.inRange(_), .pastEnd):
    return true
  default:
    return false
  }
}

extension _ClosedRange where Bound : Strideable {
  public var count: Bound.Stride {
    return lowerBound.distance(to: upperBound)
  }
  
  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != upperBound`.
  public subscript(position: Bound) -> Bound {
    _stdlibAssert(self.contains(position), "Index out of range")
    return position
  }

  public subscript(bounds: ClosedRange<Bound>) -> Self {
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
}

public struct ClosedRangeOfStrideable<
  Bound : Strideable
> : Equatable, RandomAccessCollection,
  CustomStringConvertible, CustomDebugStringConvertible, 
  _ClosedRange {

  public typealias Element = Bound
  public typealias Index = Element
  
  public init(_uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }
  
  // FIXME(compiler limitation): this typealias should be inferred.
  public typealias SubSequence = ClosedRangeOfStrideable<Bound>

  public subscript(bounds: Range<Element>) -> ClosedRangeOfStrideable<Bound> {
    fatalError("implement")
  }

  public subscript(
    bounds: RangeOfStrideable<Bound>
  ) -> RangeOfStrideable<Bound> {
    fatalError("implement")
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
  
  @warn_unused_result
  public func _customContainsEquatableElement(element: Element) -> Bool? {
    return element >= self.lowerBound && element < self.upperBound
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "ClosedRangeOfStrideable(\(String(reflecting: lowerBound))...\(String(reflecting: upperBound)))"
  }

  public // ambiguity resolution between _RangeProtocol and Collection defaults
  var isEmpty: Bool {
    return lowerBound == upperBound
  }
}

extension ClosedRangeOfStrideable : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["lowerBound": lowerBound, "upperBound": upperBound])
  }
}

@warn_unused_result
public func == <Bound>(
  lhs: ClosedRangeOfStrideable<Bound>,
  rhs: ClosedRangeOfStrideable<Bound>
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
public struct ClosedRange<
  Bound : Comparable
> : Equatable, CustomStringConvertible, CustomDebugStringConvertible,
    _ClosedRange {

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

extension ClosedRange : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["lowerBound": lowerBound, "upperBound": upperBound])
  }
}

@warn_unused_result
public func == <Bound>(
  lhs: ClosedRange<Bound>, rhs: ClosedRange<Bound>
) -> Bool {
  return
    lhs.lowerBound == rhs.lowerBound &&
    lhs.upperBound == rhs.upperBound
}

