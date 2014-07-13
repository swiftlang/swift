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

public struct RangeGenerator<
  T: ForwardIndexType
> : GeneratorType, SequenceType {
  public typealias Element = T

  @transparent public
  init(_ bounds: Range<T>) {
    self.startIndex = bounds.startIndex
    self.endIndex = bounds.endIndex
  }

  public mutating func next() -> Element? {
    if startIndex == endIndex {
      return .None
    }
    return startIndex++
  }

  // Every GeneratorType is also a single-pass SequenceType
  public typealias Generator = RangeGenerator<T>
  public func generate() -> Generator {
    return self
  }

  public var startIndex: T
  public var endIndex: T
}

public struct Range<
  T: ForwardIndexType
> : Equatable, CollectionType {

  @transparent public
  init(start: T, end: T) {
    _startIndex = start
    _endIndex = end
  }

  public var isEmpty : Bool {
    return startIndex == endIndex
  }

  public typealias Index = T
  public typealias Slice = Range<T>
  public typealias _Element = T
  
  public subscript(i: T) -> T {
    return i
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
    _fatalError("It shouldn't be possible to call this function'")
  }
  
  //===--------------------------------------------------------------------===//
  
  public typealias Generator = RangeGenerator<T>
  public func generate() -> RangeGenerator<T> {
    return Generator(self)
  }

  public var startIndex: T {
    get {
      return _startIndex
    }
    set(newValue) {
      _startIndex = newValue
    }
  }

  public var endIndex: T {
    get {
      return _endIndex
    }
    set(newValue) {
      _endIndex = newValue
    }
  }
  
  var _startIndex: T
  var _endIndex: T
}

public func ==<T>(lhs: Range<T>, rhs: Range<T>) -> Bool {
  return lhs._startIndex == rhs._startIndex &&
      lhs._endIndex == rhs._endIndex
}

public func count<I: RandomAccessIndexType>(r: Range<I>) -> I.Distance {
  return r.startIndex.distanceTo(r.endIndex)
}


/// Forms a half open range including the minimum value but excluding the
/// maximum value.
@transparent
@availability(
  *, unavailable, message="half-open range operator .. has been renamed to ..<")
public func .. <Pos : ForwardIndexType> (min: Pos, max: Pos) -> Range<Pos> {
  return Range(start: min, end: max)
}

/// Forms a half-open range that contains `minimum`, but not
/// `maximum`.
@transparent public
func ..< <Pos : ForwardIndexType> (minimum: Pos, maximum: Pos) -> Range<Pos> {
  return Range(start: minimum, end: maximum)
}


/// Forms a closed range that contains both `minimum` and `maximum`.
@transparent public
func ... <Pos : ForwardIndexType> (
  minimum: Pos, maximum: Pos
) -> Range<Pos> {
  return Range(start: minimum, end: maximum.successor())
}

//
// Pattern matching support for ranges
//
// Ranges can be used to match values contained within the range, e.g.:
// switch x {
// case 0...10:
//   println("single digit")
// case _:
//   println("too big")
// }

@infix public
func ~= <
  T: RandomAccessIndexType where T.Distance : SignedIntegerType
>(x: Range<T>, y: T) -> Bool {
  let a = x.startIndex.distanceTo(y) >= 0
  let b = y.distanceTo(x.endIndex) > 0
  return a && b
}


extension Range {
  /// Return an array containing the results of calling
  /// `transform(x)` on each element `x` of `self`.
  public func map<U>(transform: (T)->U) -> [U] {
    return Swift.map(self, transform)
  }
}
