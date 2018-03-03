//===--- RelativeRangeOffset.swift ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A type that can be used to get a slice of a collection relative to the
/// collections start and end indices.
public protocol RelativeOffsetRangeExpression {
  /// A type that represents an offset range to slice a collection relativly.
  func relativeOffsetRange() -> RelativeOffsetRange
}

/// Returns a relative offset range that represents a slice of a collection
/// given two offset values relative to the collections start and/or end 
/// indices.
///
/// You can use the RelativeOffsetRange to represent a slice of a collection 
/// relative with offset values relative the collections start/end indices.
/// If the offset value is greater or equal to 0 it will be relative to the 
/// `startIndex`, else it will be relative to the `endIndex`. For example:
///
///     let exceptLastFour = ..<(-4)
///
///     let r1 = exceptLastFour.relativeAsOffset()
///     // r1 == 0..<-4
///
/// The `r1` range is bounded on the lower end by `0` because that is the
/// offset that does not change `startIndex`. In the next example we'll see
/// how a partial range from works:
///
///     let greeting = "Hello, Swift!"
///     let greeting = numbers[offset: (-6)...]
///     // greeting == "Swift!"
///
/// Use this method only if you need a range that will be used to slice a 
/// `collection` with offsets relative to the start and/or end indices. To
/// access a relative offset slice of a collection using a relative range 
/// offset expression, use the collection's generic subscript that uses a 
/// relative offset range expression as its parameter.
///
///     let numbersPrefix = numbers[offset: upToFour]
///     // numbersPrefix == [10, 20, 30, 40]
///
/// - Returns: An offset range suitable for slicing `collection` as an offset. 
///   The returned range is *not* guaranteed to be inside the bounds of 
///   `collection`. Callers should apply the same preconditions to the return 
///   value as they would to a range provided directly by the user.
public struct RelativeOffsetRange : RelativeOffsetRangeExpression {
  /// The lower base of the range
  public let lowerBound: Int
  /// The upper base of the range. If `nil` upperBound represents the endIndex.
  public let upperBound: Int?
  
  public init(_ lb: Int, _ ub: Int?) {
    lowerBound = lb
    upperBound = ub
  }
  
  public func relative<C: Collection>(to c: C) -> Range<Int>
  where C.Index == Int {
    let start = lowerBound < 0
      ? -c.index(c.startIndex, offsetBy: -lowerBound)
      : c.index(c.startIndex, offsetBy: lowerBound)
      
    let end = upperBound == nil || upperBound! < 0
      ? -c.index(c.startIndex, offsetBy: -(upperBound ?? 0))
      : c.index(c.startIndex, offsetBy: upperBound!)
      
    return start..<end
  }
  
  public func relativeOffsetRange() -> RelativeOffsetRange {
    return self
  }
}

extension RelativeOffsetRange : Equatable {
  public static func ==(
    lhs: RelativeOffsetRange, rhs: RelativeOffsetRange
  ) -> Bool {
    return lhs.lowerBound == rhs.lowerBound
    && lhs.upperBound == rhs.upperBound
  }
}

extension Range : RelativeOffsetRangeExpression where Bound == Int {
  public func relativeOffsetRange() -> RelativeOffsetRange {
    return RelativeOffsetRange(lowerBound, upperBound)
  }
}
extension ClosedRange : RelativeOffsetRangeExpression where Bound == Int {
  public func relativeOffsetRange() -> RelativeOffsetRange {
    return RelativeOffsetRange(
      lowerBound, upperBound == -1 ? nil : upperBound + 1)
  }
}
extension PartialRangeFrom : RelativeOffsetRangeExpression where Bound == Int {
  public func relativeOffsetRange() -> RelativeOffsetRange {
    return RelativeOffsetRange(lowerBound, nil)
  }
}
extension PartialRangeUpTo : RelativeOffsetRangeExpression where Bound == Int {
  public func relativeOffsetRange() -> RelativeOffsetRange {
    return RelativeOffsetRange(0, upperBound)
  }
}
extension PartialRangeThrough : RelativeOffsetRangeExpression 
where Bound == Int {
  public func relativeOffsetRange() -> RelativeOffsetRange {
    return RelativeOffsetRange(0, upperBound == -1 ? nil : upperBound + 1)
  }
}

extension Collection {
  public func _relativeAsOffset<R: RelativeOffsetRangeExpression>(
    _ offset: R
  ) -> Range<Index> {
    let r = offset.relativeOffsetRange()

    let startOffset = r.lowerBound < 0
      ? count + r.lowerBound
      : r.lowerBound
    let endOffset = r.upperBound == nil || r.upperBound! < 0
      ? count + (r.upperBound ?? 0)
      : r.upperBound!

    let start = index(startIndex, offsetBy: startOffset)
    let end = index(start, offsetBy: endOffset - startOffset)

    return start..<end
  }

  public func _relativeAsOffset(_ offset: Int) -> Index {
    let baseOffset = offset < 0
      ? count + offset
      : offset

    return index(startIndex, offsetBy: baseOffset)
  }

  /// Accesses a contiguous subrange of the collection's elements with a range
  /// that has offsets relative to the start and/or end indices.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection uses. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  ///
  /// - Parameter offset: A range of values that will offset the collections 
  ///   starting index to form a new range of indices relative to the 
  ///   collection.
  public subscript<R: RelativeOffsetRangeExpression>(
    offset offset: R
  ) -> SubSequence {
      return self[_relativeAsOffset(offset)]
  }

  /// Accesses an element of the collection at a particular offset. Either from
  /// the start or end of the collection. Where a negative offset would imply
  /// from the end and a positive offset would imply an offset from the start.
  public subscript(offset offset: Int) -> Element {
    return self[_relativeAsOffset(offset)]
  }
}

extension MutableCollection {
  /// Accesses an element of the collection at a particular offset. Either from
  /// the start or end of the collection. Where a negative offset would imply
  /// from the end and a positive offset would imply an offset from the start.
  public subscript(offset offset: Int) -> Element {
    get {
      return self[_relativeAsOffset(offset)]
    }
    set {
      self[_relativeAsOffset(offset)] = newValue
    }
  }
}

extension RangeReplaceableCollection {
  /// Accesses a contiguous subrange of the collection's elements with a range
  /// that has offsets relative to the start and/or end indices.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection uses. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  ///
  /// - Parameter offset: A range of values that will offset the collections 
  ///   starting index to form a new range of indices relative to the 
  ///   collection.
  public subscript<R: RelativeOffsetRangeExpression>(
    offset offset: R
  ) -> SubSequence {
    get {
      return self[_relativeAsOffset(offset)]
    }
    set {
      replaceSubrange(_relativeAsOffset(offset), with: newValue)
    }
  }
}

public func ...-(lhs: Int, rhs: Int) -> RelativeOffsetRange {
  return RelativeOffsetRange(lhs, rhs == 1 ? nil : -rhs + 1)
}

public func ..<-(lhs: Int, rhs: Int) -> RelativeOffsetRange {
  return RelativeOffsetRange(lhs, -rhs)
}

public prefix func ...-(bound: Int) -> RelativeOffsetRange {
  return RelativeOffsetRange(0, bound == 1 ? nil : -bound + 1)
}

public prefix func ..<-(bound: Int) -> RelativeOffsetRange {
  return RelativeOffsetRange(0, -bound)
}