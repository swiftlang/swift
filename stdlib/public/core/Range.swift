//===- Range.swift.gyb ----------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A generator over the elements of `Range<T>`
public struct RangeGenerator<
  T : ForwardIndexType
> : GeneratorType, SequenceType {
  /// The type of element returned by `next()`.
  public typealias Element = T

  /// Construct an instance that traverses the elements of `bounds`
  @transparent public
  init(_ bounds: Range<T>) {
    self.startIndex = bounds.startIndex
    self.endIndex = bounds.endIndex
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  public mutating func next() -> Element? {
    if startIndex == endIndex {
      return .None
    }
    return startIndex++
  }

  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Generator = RangeGenerator<T>

  /// `RangeGenerator` is also a `SequenceType`, so it
  /// `generate`\ 's a copy of itself
  public func generate() -> Generator {
    return self
  }

  /// The lower bound of the remaining range.
  public var startIndex: T
  
  /// The upper bound of the remaining range; not included in the
  /// generated sequence.
  public var endIndex: T
}

/// A collection of consecutive discrete index values.
///
/// :param: `T` is both the element type and the index type of the
///   collection.
///
/// Like other collections, a range containing one element has an
/// `endIndex` that is the successor of its `startIndex`; and an empty
/// range has `startIndex == endIndex`.
///
/// Axiom: for any `Range` `r`, `r[i] == i`.
///
/// Therefore, if `T` has a maximal value, it can serve as an
/// `endIndex`, but can never be contained in a `Range<T>`.
///
/// It also follows from the axiom above that `(-99..<100)[0] == 0`.
/// To prevent confusion (because some expect the result to be `-99`),
/// in a context where `T` is known to be an integer type,
/// subscripting with `T` is a compile-time error::
///
///   // error: could not find an overload for 'subscript'...
///   println( Range<Int>(start:-99, end:100)[0] )
///
/// However, subscripting that range still works in a generic context::
///
///   func brackets<T :ForwardIndexType>(x: Range<T>, i: T) -> T {
///     return x[i] // Just forward to subscript
///   }
///   println(brackets(Range<Int>(start:-99, end:100), 0)) // prints 0
public struct Range<
  T : ForwardIndexType
> : Equatable, CollectionType,
    CustomStringConvertible, CustomDebugStringConvertible {

  /// Construct a copy of `x`
  public init(_ x: Range) {
    // This initializer exists only so that we can have a
    // debugDescription that actually constructs the right type when
    // evaluated
    self = x
  }

  /// Construct a range with `startIndex == start` and `endIndex ==
  /// end`.
  @transparent public
  init(start: T, end: T) {
    _startIndex = start
    _endIndex = end
  }

  /// `true` iff the range is empty, i.e. `startIndex == endIndex`
  public var isEmpty : Bool {
    return startIndex == endIndex
  }

  /// A type that represents a valid position in the collection.
  /// 
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = T
  public typealias _Element = T
  
  /// Access the element at `position`.
  ///
  /// Requires: `position` is a valid position in `self` and
  /// `position != endIndex`.
  public subscript(position: T) -> T {
    _debugPrecondition(position != endIndex, "Index out of range")
    return position
  }

  //===--------------------------------------------------------------------===//
  // Overloads for subscript that allow us to make subscripting fail
  // at compile time, outside a generic context, when T is an IntegerType
  // type. The current language design gives us no way to force r[0]
  // to work "as expected" (return the first element of the range) for
  // an arbitrary Range<Int>, so instead we make it ambiguous.  Same
  // goes for slicing.  The error message will be poor but at least it
  // is a compile-time error.
  public subscript(_: T._DisabledRangeIndex) -> T {
    _sanityCheckFailure("It shouldn't be possible to call this function'")
  }
  
  //===--------------------------------------------------------------------===//
  
  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Generator = RangeGenerator<T>

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// Complexity: O(1)
  public func generate() -> RangeGenerator<T> {
    return Generator(self)
  }

  /// The range's lower bound
  ///
  /// Identical to `endIndex` in an empty range.
  public var startIndex: T {
    get {
      return _startIndex
    }
    set(newValue) {
      _startIndex = newValue
    }
  }

  /// The range's upper bound
  ///
  /// `endIndex` is not a valid argument to `subscript`, and is always
  /// reachable from `startIndex` by zero or more applications of
  /// `successor()`.
  public var endIndex: T {
    get {
      return _endIndex
    }
    set(newValue) {
      _endIndex = newValue
    }
  }

  /// A textual representation of `self`.
  public var description: String {
    return "\(startIndex)..<\(endIndex)"
  }
  
  /// A textual representation of `self`, suitable for debugging.
  public var debugDescription: String {
    return "Range(\(String(reflecting: startIndex))..<\(String(reflecting: endIndex)))"
  }
  
  
  var _startIndex: T
  var _endIndex: T
}

public func ==<T>(lhs: Range<T>, rhs: Range<T>) -> Bool {
  return lhs._startIndex == rhs._startIndex &&
      lhs._endIndex == rhs._endIndex
}

/// Forms a half-open range that contains `minimum`, but not
/// `maximum`.
@transparent
public func ..< <Pos : ForwardIndexType> (minimum: Pos, maximum: Pos) -> Range<Pos> {
  return Range(start: minimum, end: maximum)
}

/// Forms a closed range that contains both `minimum` and `maximum`.
@transparent
public func ... <Pos : ForwardIndexType> (
  minimum: Pos, maximum: Pos
) -> Range<Pos> {
  return Range(start: minimum, end: maximum.successor())
}

//===--- Prefer Ranges to Intervals, and add checking ---------------------===//

/// Forms a half-open range that contains `start`, but not
/// `end`.  Requires: `start <= end`
@transparent
public func ..< <Pos : ForwardIndexType where Pos: Comparable> (
  start: Pos, end: Pos
) -> Range<Pos> {
  _precondition(start <= end, "Can't form Range with end < start")
  return Range(start: start, end: end)
}

/// Forms a closed range that contains both `start` and `end`.
/// Requres: `start <= end`
@transparent
public func ... <Pos : ForwardIndexType where Pos: Comparable> (
  start: Pos, end: Pos
) -> Range<Pos> {
  _precondition(start <= end, "Can't form Range with end < start")
  _precondition(end.successor() > end, "Range end index has no valid successor")
  return Range(start: start, end: end.successor())
}

public func ~= <I : ForwardIndexType where I: Comparable> (
  pattern: Range<I>, value: I
) -> Bool {
  // convert to an interval and check that.
  return (pattern.startIndex..<pattern.endIndex).contains(value)
}

extension Range {
  /// Return an array containing the results of calling
  /// `transform(x)` on each element `x` of `self`.
  public func map<U>(transform: (T)->U) -> [U] {
    return Swift.map(self, transform)
  }
}
