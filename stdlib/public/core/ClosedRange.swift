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
  /// Creates the "past the end" position.
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

/// A closed range that forms a collection of consecutive values.
///
/// You create a `CountableClosedRange` instance by using the closed range
/// operator (`...`).
///
///     let throughFive = 0...5
///
/// A `CountableClosedRange` instance contains both its lower bound and its
/// upper bound.
///
///     print(throughFive.contains(3))      // Prints "true"
///     print(throughFive.contains(10))     // Prints "false"
///     print(throughFive.contains(5))      // Prints "true"
///
/// Because a closed range includes its upper bound, a closed range whose lower
/// bound is equal to the upper bound contains one element. Therefore, a
/// `CountableClosedRange` instance cannot represent an empty range.
///
///     let zeroInclusive = 0...0
///     print(zeroInclusive.isEmpty)
///     // Prints "false"
///     print(zeroInclusive.count)
///     // Prints "1"
///
/// You can use a `for`-`in` loop or any sequence or collection method with a
/// countable range. The elements of the range are the consecutive values from
/// its lower bound up to, and including, its upper bound.
///
///     for n in throughFive.suffix(3) {
///         print(n)
///     }
///     // Prints "3"
///     // Prints "4"
///     // Prints "5"
///
/// You can create a countable range over any type that conforms to the
/// `Strideable` protocol and uses an integer as its associated `Stride` type.
/// By default, Swift's integer and pointer types are usable as the bounds of
/// a countable range.
///
/// Because floating-point types such as `Float` and `Double` are their own
/// `Stride` types, they cannot be used as the bounds of a countable range. If
/// you need to test whether values are contained within a closed interval
/// bound by floating-point values, see the `ClosedRange` type. If you need to
/// iterate over consecutive floating-point values, see the
/// `stride(from:through:by:)` function.
///
/// - SeeAlso: `CountableRange`, `ClosedRange`, `Range`
public struct CountableClosedRange<
  // WORKAROUND rdar://25214598 - should be just Bound : Strideable
  Bound : protocol<_Strideable, Comparable>
  where
  Bound.Stride : SignedInteger
> : RandomAccessCollection {

  /// The range's lower bound.
  public let lowerBound: Bound

  /// The range's upper bound.
  ///
  /// `upperBound` is always reachable from `lowerBound` by zero or
  /// more applications of `index(after:)`.
  public let upperBound: Bound

  /// The element type of the range; the same type as the range's bounds.
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

  /// The position of the first element in the range.
  public var startIndex: ClosedRangeIndex<Bound> {
    return ClosedRangeIndex(lowerBound)
  }

  /// The range's "past the end" position---that is, the position one greater
  /// than the last valid subscript argument.
  public var endIndex: ClosedRangeIndex<Bound> {
    return ClosedRangeIndex()
  }

  public func index(after i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range checks and tests.
    return i._successor(upperBound: upperBound)
  }

  public func index(before i: Index) -> Index {
    // FIXME: swift-3-indexing-model: range checks and tests.
    return i._predecessor(upperBound: upperBound)
  }

  // FIXME: swift-3-indexing-model: implement O(1) `index(_:offsetBy:)`
  // and `distance(from:to:)`, and write tests for them.

  /// Accesses the element at specified position.
  ///
  /// You can subscript a collection with any valid index other than the
  /// collection's end index. The end index refers to the position one past
  /// the last element of a collection, so it doesn't correspond with an
  /// element.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the range, and must not equal the range's end
  ///   index.
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
  /// Because this initializer does not perform any checks, it should be used
  /// as an optimization only when you are absolutely certain that `lower` is
  /// less than or equal to `upper`. Using the closed range operator (`...`)
  /// to form `CountableClosedRange` instances is preferred.
  ///
  /// - Parameter bounds: A tuple of the lower and upper bounds of the range.
  public init(uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }

  public func _customContainsEquatableElement(_ element: Bound) -> Bool? {
    return element >= self.lowerBound && element <= self.upperBound
  }

  /// A Boolean value indicating whether the range contains no elements.
  ///
  /// Because a closed range cannot represent an empty range, this property is
  /// always `false`.
  public var isEmpty: Bool {
    return false
  }
}

/// An interval over a comparable type, from a lower bound up to, and
/// including, an upper bound.
///
/// You create instances of `ClosedRange` by using the closed range operator
/// (`...`).
///
///     let lowercase = "a"..."z"
///
/// You can use a `ClosedRange` instance to quickly check if a value is
/// contained in a particular range of values. For example:
///
///     print(lowercase.contains("c"))      // Prints "true"
///     print(lowercase.contains("5"))      // Prints "false"
///     print(lowercase.contains("z"))      // Prints "true"
///
/// Unlike `Range`, instances of `ClosedRange` cannot represent an empty
/// interval.
///
///     let lowercaseA = "a"..."a"
///     print(lowercaseA.isEmpty)
///     // Prints "false"
///
/// - SeeAlso: `CountableRange`, `Range`, `CountableClosedRange`
@_fixed_layout
public struct ClosedRange<
  Bound : Comparable
> {

  /// Creates an instance with the given bounds.
  ///
  /// Because this initializer does not perform any checks, it should be used
  /// as an optimization only when you are absolutely certain that `lower` is
  /// less than or equal to `upper`. Using the closed range operator (`...`)
  /// to form `ClosedRange` instances is preferred.
  ///
  /// - Parameter bounds: A tuple of the lower and upper bounds of the range.
  @inline(__always)
  public init(uncheckedBounds bounds: (lower: Bound, upper: Bound)) {
    self.lowerBound = bounds.lower
    self.upperBound = bounds.upper
  }

  /// The range's lower bound.
  public let lowerBound: Bound

  /// The range's upper bound.
  public let upperBound: Bound

  /// Returns a Boolean value indicating whether the given element is contained
  /// within the range.
  ///
  /// A `ClosedRange` instance contains both its lower and upper bound.
  /// `element` is contained in the range if it is between the two bounds or
  /// equal to either bound.
  ///
  /// - Parameter element: The element to check for containment.
  /// - Returns: `true` if `element` is contained in the range; otherwise,
  ///   `false`.
  public func contains(_ element: Bound) -> Bool {
    return element >= self.lowerBound && element <= self.upperBound
  }

  /// A Boolean value indicating whether the range contains no elements.
  ///
  /// Because a closed range cannot represent an empty range, this property is
  /// always `false`.
  public var isEmpty: Bool {
    return false
  }
}

/// Returns a closed range that contains both of its bounds.
///
/// For example:
///
///     let lowercase = "a"..."z"
///     print(lowercase.contains("z"))
///     // Prints "true"
///
/// - Parameters:
///   - minimum: The lower bound for the range.
///   - maximum: The upper bound for the range.
@_transparent
public func ... <Bound : Comparable> (minimum: Bound, maximum: Bound)
  -> ClosedRange<Bound> {
  _precondition(
    minimum <= maximum, "Can't form Range with upperBound < lowerBound")
  return ClosedRange(uncheckedBounds: (lower: minimum, upper: maximum))
}

/// Returns a countable closed range that contains both of its bounds.
///
/// For example:
/// 
///     let singleDigits = 0...9
///     print(singleDigits.contains(9))
///     // Prints "true"
///
/// - Parameters:
///   - minimum: The lower bound for the range.
///   - maximum: The upper bound for the range.
@_transparent
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

extension ClosedRange {
  @available(*, unavailable, renamed: "lowerBound")
  public var startIndex: Bound {
    Builtin.unreachable()
  }

  @available(*, unavailable, renamed: "upperBound")
  public var endIndex: Bound {
    Builtin.unreachable()
  }
}
