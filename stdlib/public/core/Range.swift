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

// FIXME: swift-3-indexing-model: this whole file.

public struct RangeOfStrideable<
  Element : Strideable
> : Equatable, Collection,
  CustomStringConvertible, CustomDebugStringConvertible {

  /// Construct a copy of `x`.
  @inline(__always)
  public init(_ x: RangeOfStrideable<Element>) {
    // This initializer exists only so that we can have a
    // debugDescription that actually constructs the right type when
    // evaluated
    self = x
  }

  /// Construct a copy of `x`.
  @inline(__always)
  public init(_ x: Range<Element>) {
    self.startIndex = x.startIndex
    self.endIndex = x.endIndex
  }

  /// Construct a range with `startIndex == start` and `endIndex ==
  /// end`.
  @inline(__always)
  internal init(_start: Element, end: Element) {
    self.startIndex = _start
    self.endIndex = end
  }

  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != endIndex`.
  public subscript(position: Element) -> Element {
    _stdlibAssert(position != endIndex, "Index out of range")
    return position
  }

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  public func makeIterator() -> RangeOfStrideableIterator<Element> {
    return RangeOfStrideableIterator(_bounds: self)
  }

  /// The range's lower bound.
  ///
  /// Identical to `endIndex` in an empty range.
  public var startIndex: Element

  /// The range's upper bound.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: Element

  @warn_unused_result
  public func _customContainsEquatableElement(element: Element) -> Bool? {
    return element >= self.startIndex && element < self.endIndex
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(startIndex)..<\(endIndex)"
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "RangeOfStrideable(\(String(reflecting: startIndex))..<\(String(reflecting: endIndex)))"
  }
}

extension RangeOfStrideable : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["startIndex": startIndex, "endIndex": endIndex])
  }
}

@warn_unused_result
public func == <Element>(
  lhs: RangeOfStrideable<Element>,
  rhs: RangeOfStrideable<Element>
) -> Bool {
  return
    lhs.startIndex == rhs.startIndex &&
    lhs.endIndex == rhs.endIndex
}

/// An iterator over the elements of `Range<Element>`.
public struct RangeOfStrideableIterator<
  Element : Strideable
> : IteratorProtocol, Sequence {

  /// Construct an instance that traverses the elements of `bounds`.
  @inline(__always)
  internal init(_bounds: RangeOfStrideable<Element>) {
    self._start = _bounds.startIndex
    self._end = _bounds.endIndex
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  public mutating func next() -> Element? {
    if _start == _end { return nil }
    let result = _start
    _start = _start.advanced(by: 1)
    return result
  }

  /// The lower bound of the remaining range.
  internal var _start: Element

  /// The upper bound of the remaining range; not included in the
  /// generated sequence.
  internal let _end: Element
}

/// A collection of consecutive discrete index values.
///
/// - parameter Element: Is both the element type and the index type of the
///   collection.
///
/// Like other collections, a range containing one element has an
/// `endIndex` that is the successor of its `startIndex`; and an empty
/// range has `startIndex == endIndex`.
///
/// Axiom: for any `Range` `r`, `r[i] == i`.
///
/// Therefore, if `Element` has a maximal value, it can serve as an
/// `endIndex`, but can never be contained in a `Range<Element>`.
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
  Element : Comparable
> : Equatable,
    CustomStringConvertible, CustomDebugStringConvertible {

  /// Construct a copy of `x`.
  @inline(__always)
  public init(_ x: Range<Element>) {
    // This initializer exists only so that we can have a
    // debugDescription that actually constructs the right type when
    // evaluated
    self = x
  }

  /// Construct a range with `startIndex == start` and `endIndex ==
  /// end`.
  @inline(__always)
  internal init(_start: Element, end: Element) {
    self.startIndex = _start
    self.endIndex = end
  }

  /// The range's lower bound.
  ///
  /// Identical to `endIndex` in an empty range.
  // FIXME: swift-3-indexing-model: rename to `start`.
  public var startIndex: Element

  /// The range's upper bound.
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  // FIXME: swift-3-indexing-model: rename to `end`.
  public var endIndex: Element

  // FIXME: does not implement a requirement in `Collection`.
  // We need to implement `_customContainsEquatableElement` instead.
  @warn_unused_result
  public func contains(element: Element) -> Bool {
    return element >= self.startIndex && element < self.endIndex
  }

  public var isEmpty: Bool {
    return startIndex == endIndex
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(startIndex)..<\(endIndex)"
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "Range(\(String(reflecting: startIndex))..<\(String(reflecting: endIndex)))"
  }
}

extension Range : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(self, children: ["startIndex": startIndex, "endIndex": endIndex])
  }
}

extension Range where Element : Strideable {
  /// Construct a copy of `x`.
  @inline(__always)
  public init(_ x: RangeOfStrideable<Element>) {
    self.startIndex = x.startIndex
    self.endIndex = x.endIndex
  }

  public var count: Element.Stride {
    return startIndex.distance(to: endIndex)
  }
}

@warn_unused_result
public func == <Element>(lhs: Range<Element>, rhs: Range<Element>) -> Bool {
  return
    lhs.startIndex == rhs.startIndex &&
    lhs.endIndex == rhs.endIndex
}

/// Forms a half-open range that contains `minimum`, but not
/// `maximum`.
@_transparent
@warn_unused_result
public func ..< <Pos : Comparable> (minimum: Pos, maximum: Pos)
  -> Range<Pos> {
  _precondition(minimum <= maximum, "Can't form Range with end < start")
  return Range(_start: minimum, end: maximum)
}

//===--- Prefer Ranges to Intervals, and add checking ---------------------===//

/// Forms a half-open range that contains `start`, but not `end`.
///
/// - Precondition: `start <= end`.
@_transparent
@warn_unused_result
public func ..< <Pos : Strideable> (
  start: Pos, end: Pos
) -> RangeOfStrideable<Pos> {
  _precondition(start <= end, "Can't form Range with end < start")
  return RangeOfStrideable(_start: start, end: end)
}

/// Forms a closed range that contains both `start` and `end`.
/// - Precondition: `start <= end`.
@_transparent
@warn_unused_result
public func ... <Pos : Strideable> (
  start: Pos, end: Pos
) -> Range<Pos> {
  _precondition(start <= end, "Can't form Range with end < start")
  _precondition(end.advanced(by: 1) > end, "Range end index has no valid successor")
  return Range(_start: start, end: end.advanced(by: 1))
}

@warn_unused_result
public func ~= <I : Comparable> (
  pattern: Range<I>, value: I
) -> Bool {
  return pattern.contains(value)
}

@warn_unused_result
public func ~= <I : Strideable> (
  pattern: RangeOfStrideable<I>, value: I
) -> Bool {
  return pattern.contains(value)
}

@available(*, unavailable, renamed="RangeOfStrideableIterator")
public struct RangeGenerator<Element> {}

extension RangeOfStrideableIterator {
  @available(*, unavailable, message="use the 'makeIterator()' method on the collection")
  public init(_ bounds: Range<Element>) {
    fatalError("unavailable function can't be called")
  }
}

extension Range {
  @available(*, unavailable, message="use the '..<' operator")
  public init(start: Element, end: Element) {
    fatalError("unavailable function can't be called")
  }
}

