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

// WORKAROUND rdar://25214598 - should be Bound : Strideable
internal enum _ClosedRangeIndex<
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> {
case pastEnd
case inRange(Bound)
}

public struct ClosedRangeIndex<
  // WORKAROUND rdar://25214598 - should be Bound : Strideable
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> : Comparable {
  internal init() { _value = .pastEnd }
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
  
  internal var _value : _ClosedRangeIndex<Bound>
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

// WORKAROUND rdar://25214598 - should be Bound : Strideable
extension _ClosedRange where Bound : _Strideable, Bound.Stride : Integer {
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

// WORKAROUND: for some reason
// IndexingIterator<CountableClosedRange> doesn't conform to
// IteratorProtocol?!
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

public struct CountableClosedRange<
  // WORKAROUND rdar://25214598 - should be just Bound : Strideable
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> : Equatable, RandomAccessCollection,
  CustomStringConvertible, CustomDebugStringConvertible, 
  _ClosedRange {

  public typealias Element = Bound
  public typealias Index = ClosedRangeIndex<Bound>
  public typealias Iterator = ClosedRangeIterator<Bound>
  //public typealias IndexDistance = Int
  // FIXME: swift-3-indexing-model: IndexDistance = Bound.Stride
  
  public func makeIterator() -> ClosedRangeIterator<Bound> {
    return ClosedRangeIterator(self)
  }

  @warn_unused_result
  public func successor(of i: Index) -> Index {
    return i._successor(upperBound: upperBound)
  }

  @warn_unused_result
  public func predecessor(of i: Index) -> Index {
    return i._predecessor(upperBound: upperBound)
  }

  public var indices: DefaultRandomAccessIndices<CountableClosedRange<Bound>> {
    return DefaultRandomAccessIndices(
      _elements: self,
      startIndex: self.startIndex,
      endIndex: self.endIndex)
  }

  public init(_uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
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
    return "CountableClosedRange(\(String(reflecting: lowerBound))...\(String(reflecting: upperBound)))"
  }

  public // ambiguity resolution between _RangeProtocol and Collection defaults
  var isEmpty: Bool {
    return false
  }

  public subscript(bounds: Range<Index>) -> RandomAccessSlice<CountableClosedRange> {
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

@warn_unused_result
public func == <Bound>(
  lhs: CountableClosedRange<Bound>,
  rhs: CountableClosedRange<Bound>
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
    return "ClosedRange(\(String(reflecting: lowerBound))...\(String(reflecting: upperBound)))"
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

/// Returns a closed range that contains `minimum` and
/// `maximum`.
///
/// - Precondition: `minimum <= maximum`.
@_transparent
@warn_unused_result
public func ... <Bound : Comparable> (minimum: Bound, maximum: Bound)
  -> ClosedRange<Bound> {
  _precondition(minimum <= maximum, "Can't form Range with end < start")
  return ClosedRange(_uncheckedBounds: (lower: minimum, upper: maximum))
}

/// Returns a closed range that contains `start` and `end`.
///
/// - Precondition: `start <= end`.
@_transparent
@warn_unused_result
// WORKAROUND rdar://25214598 - should be just Bound : Strideable
public func ... <
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> (
  start: Bound, end: Bound
) -> CountableClosedRange<Bound> {
  // FIXME: swift-3-indexing-model: tests for traps.
  _precondition(start <= end, "Can't form Range with end < start")
  return CountableClosedRange(_uncheckedBounds: (lower: start, upper: end))
}

