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

// FIXME: swift-3-indexing-model: Generalize all tests to check both
// [Closed]Range and [Closed]CountableRange.

/// A type that represents a contiguous range of any comparable value,
/// containing both their lower bounds and upper bounds.
///
/// A closed range is never empty.
public protocol ClosedRangeProtocol : RangeProtocol {}

@warn_unused_result
public func == <
  LHS : ClosedRangeProtocol, RHS : ClosedRangeProtocol 
  where LHS.Bound == RHS.Bound
>(lhs: LHS, rhs: RHS) -> Bool {
  return
    lhs.lowerBound == rhs.lowerBound &&
    lhs.upperBound == rhs.upperBound
}

extension ClosedRangeProtocol {
  @inline(__always)
  public init<
    Other: ClosedRangeProtocol where Other.Bound == Bound
  >(_ other: Other) {
    self.init(
      uncheckedBounds: (lower: other.lowerBound, upper: other.upperBound)
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

// WORKAROUND rdar://25214598 - should be Bound : Strideable
internal enum ClosedRangeProtocolIndex<
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> {
case pastEnd
case inRange(Bound)
}

/// A position in a closed range whose `Bound` is `Strideable` with
/// `Integer` `Stride`.
public struct ClosedRangeIndex<
  // WORKAROUND rdar://25214598 - should be Bound : Strideable
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> : Comparable {
  /// Creates the past-the-end position
  internal init() { _value = .pastEnd }

  /// Creates a position `p` for which `r[p] == x`.
  internal init(_ x: Bound) { _value = .inRange(x) }
  
  internal func _successor(upperBound limit: Bound) -> ClosedRangeIndex {
    switch _value {
    case .inRange(let x): return x == limit
      ? ClosedRangeIndex() : ClosedRangeIndex(x.advanced(by: 1))
    case .pastEnd: _preconditionFailure("Incrementing past end index")
    }
  }
  
  internal func _predecessor(upperBound limit: Bound) -> ClosedRangeIndex {
    switch _value {
    case .inRange(let x): return ClosedRangeIndex(x.advanced(by: -1))
    case .pastEnd: return ClosedRangeIndex(limit)
    }
  }
  
  internal func _advancedBy(
    n: Bound.Stride, upperBound limit: Bound
  ) -> ClosedRangeIndex {
    switch _value {
    case .inRange(let x):
      let d = x.distance(to: limit)
      if n <= d { return ClosedRangeIndex(x.advanced(by: n)) }
      if d - -1 == n { return ClosedRangeIndex() }
      _preconditionFailure("Advancing past end index")
    case .pastEnd:
      return n == 0
      ? self :
      ClosedRangeIndex(limit)._advancedBy(n, upperBound: limit)
    }
  }
  
  internal var _value : ClosedRangeProtocolIndex<Bound>
  internal var _dereferenced : Bound {
    switch _value {
    case .inRange(let x): return x
    case .pastEnd: _preconditionFailure("Index out of range")
    }
  }
}

public func == <B>(lhs: ClosedRangeIndex<B>, rhs: ClosedRangeIndex<B>) -> Bool {
  switch (lhs._value, rhs._value) {
  case (.inRange(let l), .inRange(let r)):
    return l == r
  case (.pastEnd, .pastEnd):
    return true
  default:
    return false
  }
}

public func < <B>(lhs: ClosedRangeIndex<B>, rhs: ClosedRangeIndex<B>) -> Bool {
  switch (lhs._value, rhs._value) {
  case (.inRange(let l), .inRange(let r)):
    return l < r
  case (.inRange(_), .pastEnd):
    return true
  default:
    return false
  }
}

/// Closed ranges whose `Bound` is `Strideable` with `Integer` `Stride`
/// have all the capabilities of `RandomAccessCollection`s, where
/// elements of the collection are the values contained in the range.
extension ClosedRangeProtocol 
// WORKAROUND rdar://25214598 - should be Bound : Strideable
  where Bound : _Strideable, Bound.Stride : Integer {

  /// The number of values contained in `self`. 
  public var count: Bound.Stride {
    return lowerBound.distance(to: upperBound)
  }
  
  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != upperBound`.
  public subscript(position: ClosedRangeIndex<Bound>) -> Bound {
    return position._dereferenced
  }

  public var startIndex: ClosedRangeIndex<Bound> {
    return ClosedRangeIndex(lowerBound)
  }
  
  public var endIndex: ClosedRangeIndex<Bound> {
    return ClosedRangeIndex()
  }
}

// WORKAROUND: needed because of rdar://25584401
/// A position in a closed range whose `Bound` is `Strideable` with
/// `Integer` `Stride`.
public struct ClosedRangeIterator<
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> : IteratorProtocol, Sequence {

  init(_ r: CountableClosedRange<Bound>) {
    _nextResult = r.lowerBound
    _upperBound = r.upperBound
  }
  
  public func makeIterator() -> ClosedRangeIterator {
    return self
  }

  public mutating func next() -> Bound? {
    let r = _nextResult
    if let x = r {
      _nextResult = x == _upperBound ? nil : x.advanced(by: 1)
    }
    return r
  }
  internal var _nextResult: Bound?
  internal let _upperBound: Bound
}

/// A closed range whose `Bound` is `Strideable` with `Integer`
/// `Stride` and conforms to RandomAccessCollection.
public struct CountableClosedRange<
  // WORKAROUND rdar://25214598 - should be just Bound : Strideable
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> : Equatable, RandomAccessCollection,
  CustomStringConvertible, CustomDebugStringConvertible, 
  ClosedRangeProtocol {

  /// A type that represents a position in the range.
  public typealias Index = ClosedRangeIndex<Bound>
  
  // WORKAROUND: needed because of rdar://25584401
  public typealias Iterator = ClosedRangeIterator<Bound>
  //public typealias IndexDistance = Int
  // FIXME: swift-3-indexing-model: IndexDistance = Bound.Stride
  
  // WORKAROUND: needed because of rdar://25584401
  public func makeIterator() -> ClosedRangeIterator<Bound> {
    return ClosedRangeIterator(self)
  }

  /// Returns the position immediately after `i`.
  @warn_unused_result
  public func successor(of i: Index) -> Index {
    return i._successor(upperBound: upperBound)
  }

  /// Returns the position immediately preceding `i`.
  @warn_unused_result
  public func predecessor(of i: Index) -> Index {
    return i._predecessor(upperBound: upperBound)
  }

  public // WORKAROUND: needed because of rdar://25584401
  var indices: DefaultRandomAccessIndices<CountableClosedRange<Bound>> {
    return DefaultRandomAccessIndices(
      _elements: self,
      startIndex: self.startIndex,
      endIndex: self.endIndex)
  }

  /// Creates an instance with the given bounds.
  ///
  /// - Note: as this initializer does not check its precondition, it
  ///   should be used as an optimization only, when one is absolutely
  ///   certain that `lower <= upper`.  In general, the `...`
  ///   operator is to be preferred for forming closed ranges.
  ///
  /// - Precondition: `lower <= upper`
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
  /// `upperBound` is always reachable from `lowerBound` by zero or
  /// more applications of `successor()`.
  public let upperBound: Bound
  
  @warn_unused_result
  public func _customContainsEquatableElement(element: Bound) -> Bool? {
    return element >= self.lowerBound && element < self.upperBound
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "CountableClosedRange(\(String(reflecting: lowerBound))...\(String(reflecting: upperBound)))"
  }

  public // ambiguity resolution between RangeProtocol and Collection defaults
  var isEmpty: Bool {
    return false
  }

  public subscript(
    bounds: Range<Index>
  ) -> RandomAccessSlice<CountableClosedRange> {
    return RandomAccessSlice(base: self, bounds: bounds)
  }
}

extension CountableClosedRange : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self,
      children: ["lowerBound": lowerBound, "upperBound": upperBound])
  }
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
    ClosedRangeProtocol {

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
  public let upperBound: Bound

  // FIXME: does not implement a requirement in `Collection`.
  // We need to implement `_customContainsEquatableElement` instead.
  @warn_unused_result
  public func contains(element: Bound) -> Bool {
    return element >= self.lowerBound && element <= self.upperBound
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(lowerBound)...\(upperBound)"
  }

  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "ClosedRange(\(String(reflecting: lowerBound))"
    + "...\(String(reflecting: upperBound)))"
  }
}

extension ClosedRange : CustomReflectable {
  public var customMirror: Mirror {
    return Mirror(
      self, children: ["lowerBound": lowerBound, "upperBound": upperBound])
  }
}

/// Returns a closed range that contains `minimum` and
/// `maximum`.
///
/// - Precondition: `minimum <= maximum`.
@_transparent
@warn_unused_result
public func ... <Bound : Comparable> (minimum: Bound, maximum: Bound)
  -> ClosedRange<Bound> {
  _precondition(
    minimum <= maximum, "Can't form Range with upperBound < lowerBound")
  return ClosedRange(uncheckedBounds: (lower: minimum, upper: maximum))
}

/// Returns a closed range that contains `minimum` and `maximum`.
///
/// - Precondition: `minimum <= maximum`.
@_transparent
@warn_unused_result
public func ... <
  // WORKAROUND rdar://25214598 - should be just Bound : Strideable
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> (
  minimum: Bound, maximum: Bound
) -> CountableClosedRange<Bound> {
  // FIXME: swift-3-indexing-model: tests for traps.
  _precondition(
    minimum <= maximum, "Can't form Range with upperBound < lowerBound")
  return CountableClosedRange(uncheckedBounds: (lower: minimum, upper: maximum))
}

