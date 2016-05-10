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

// WORKAROUND rdar://25214598 - should be Bound : Strideable
internal enum _ClosedRangeIndexRepresentation<
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
> {
  case pastEnd
  case inRange(Bound)
}

// FIXME(ABI)(compiler limitation): should be a nested type in
// `ClosedRange`.
/// A position in a `CountableClosedRange` instance.
public struct ClosedRangeIndex<
  // WORKAROUND rdar://25214598 - should be Bound : Strideable
  Bound : Comparable where Bound : _Strideable, Bound.Stride : Integer
  // swift-3-indexing-model: should conform to _Strideable, otherwise
  // CountableClosedRange is not interchangeable with CountableRange in all
  // contexts.
> : Comparable {
  /// Creates the past-the-end position.
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
  
  internal func _advanced(
    by n: Bound.Stride, upperBound limit: Bound
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
      ClosedRangeIndex(limit)._advanced(by: n, upperBound: limit)
    }
  }
  
  internal var _value: _ClosedRangeIndexRepresentation<Bound>
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

// WORKAROUND: needed because of rdar://25584401
/// An iterator over the elements of a `CountableClosedRange` instance.
public struct ClosedRangeIterator<
  Bound : protocol<_Strideable, Comparable>
  where
  Bound.Stride : SignedInteger
> : IteratorProtocol, Sequence {

  internal init(_range r: CountableClosedRange<Bound>) {
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

/// A closed range that forms a collection of consecutive `Strideable` values.
///
/// A `CountableClosedRange` contains both its `lowerBound` and its
/// `upperBound`. A `CountableClosedRange` with one element has
/// `lowerBound == upperBound`. `CountableClosedRange` instances are never
/// empty.
///
/// - SeeAlso: `CountableRange`, `ClosedRange`, `Range`
public struct CountableClosedRange<
  // WORKAROUND rdar://25214598 - should be just Bound : Strideable
  Bound : protocol<_Strideable, Comparable>
  where
  Bound.Stride : SignedInteger
> : RandomAccessCollection {

  /// The range's lower bound.
  ///
  /// Identical to `upperBound` in an empty range.
  public let lowerBound: Bound

  /// The range's upper bound.
  ///
  /// `upperBound` is always reachable from `lowerBound` by zero or
  /// more applications of `index(after:)`.
  public let upperBound: Bound

  public typealias Element = Bound

  /// A type that represents a position in the range.
  public typealias Index = ClosedRangeIndex<Bound>

  public typealias IndexDistance = Bound.Stride

  // WORKAROUND: needed because of rdar://25584401
  public typealias Iterator = ClosedRangeIterator<Bound>

  // WORKAROUND: needed because of rdar://25584401
  public func makeIterator() -> ClosedRangeIterator<Bound> {
    return ClosedRangeIterator(_range: self)
  }

  public var startIndex: ClosedRangeIndex<Bound> {
    return ClosedRangeIndex(lowerBound)
  }

  public var endIndex: ClosedRangeIndex<Bound> {
    return ClosedRangeIndex()
  }

  /// Returns the position immediately after `i`.
  @warn_unused_result
  public func index(after i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range checks and tests.
    return i._successor(upperBound: upperBound)
  }

  /// Returns the position immediately preceding `i`.
  @warn_unused_result
  public func index(before i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range checks and tests.
    return i._predecessor(upperBound: upperBound)
  }

  // FIXME: swift-3-indexing-model: implement O(1) `index(_:offsetBy:)`
  // and `distance(from:to:)`, and write tests for them.

  /// Access the element at `position`.
  ///
  /// - Precondition: `position` is a valid position in `self` and
  ///   `position != upperBound`.
  public subscript(position: ClosedRangeIndex<Bound>) -> Bound {
    // FIXME: swift-3-indexing-model: range checks and tests.
    return position._dereferenced
  }

  public subscript(bounds: Range<Index>)
    -> RandomAccessSlice<CountableClosedRange<Bound>> {
    return RandomAccessSlice(base: self, bounds: bounds)
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
  /// - Precondition: `lower <= upper`
  public init(uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }

  @warn_unused_result
  public func _customContainsEquatableElement(_ element: Bound) -> Bool? {
    return element >= self.lowerBound && element <= self.upperBound
  }

  /// Returns `true` iff `self.contains(x)` is `false` for all values of `x`.
  public var isEmpty: Bool {
    return false
  }
}

/// An interval over a `Comparable` type, from a lower bound up to and
/// including an upper bound. Cannot represent an empty interval.
///
/// Use a `ClosedRange` to quickly check if a `Comparable` value is contained
/// in a particular range of values. For example:
///
///     let lowercase: ClosedRange = "a"..."z"
///     lowercase.contains("c")    // true
///     lowercase.contains("5")    // false
///     lowercase.contains("z")    // true
@_fixed_layout
public struct ClosedRange<
  Bound : Comparable
> {

  /// Creates a range with `lowerBound == lower` and `upperBound ==
  /// upper`.
  ///
  /// - Note: as this initializer does not check its precondition, it
  ///   should be used as an optimization only, when one is absolutely
  ///   certain that `lower <= upper`.  In general, the `...`
  ///   operator is to be preferred for forming closed ranges.
  /// - Precondition: `lower <= upper`
  @inline(__always)
  public init(uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }

  /// The range's lower bound.
  public let lowerBound: Bound

  /// The range's upper bound.
  public let upperBound: Bound

  /// Returns `true` iff `element` is between `lowerBound` and `upperBound` or
  /// equal to either bound.
  @warn_unused_result
  public func contains(_ element: Bound) -> Bool {
    return element >= self.lowerBound && element <= self.upperBound
  }

  /// Returns `true` iff `self.contains(x)` is `false` for all values of `x`.
  public var isEmpty: Bool {
    return false
  }
}

/// Returns a closed range that contains `minimum` and `maximum`.
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
  Bound : protocol<_Strideable, Comparable>
  where
  Bound.Stride : SignedInteger
> (
  minimum: Bound, maximum: Bound
) -> CountableClosedRange<Bound> {
  // FIXME: swift-3-indexing-model: tests for traps.
  _precondition(
    minimum <= maximum, "Can't form Range with upperBound < lowerBound")
  return CountableClosedRange(uncheckedBounds: (lower: minimum, upper: maximum))
}

