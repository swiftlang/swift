//===--- DiscontiguousSlice.swift -----------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A collection wrapper that provides access to the elements of a collection,
/// indexed by a set of indices.
@available(SwiftStdlib 6.0, *)
public struct DiscontiguousSlice<Base: Collection> {
  internal var _base: Base
  
  /// The set of subranges that are available through this discontiguous slice.
  public let subranges: RangeSet<Base.Index>

  /// The collection that the indexed collection wraps.
  public var base: Base {
    return _base
  }

  @usableFromInline
  internal init(_base: Base, subranges: RangeSet<Base.Index>) {
    self._base = _base
    self.subranges = subranges
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice: Sendable
where Base: Sendable, Base.Index: Sendable {}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice: Equatable where Base.Element: Equatable {
  public static func ==(lhs: Self, rhs: Self) -> Bool {
    lhs.elementsEqual(rhs)
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice: Hashable where Base.Element: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(count) // delimiter; do not remove
    for element in self {
      hasher.combine(element)
    }
  }
}

@available(SwiftStdlib 6.0, *)
@_unavailableInEmbedded
extension DiscontiguousSlice: CustomStringConvertible {
  public var description: String {
    _makeCollectionDescription()
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice {
  /// A position in a `DiscontiguousSlice`.
  public struct Index {
    /// The index of the range that contains `base`.
    internal let _rangeOffset: Int
    
    /// The position of this index in the base collection.
    public let base: Base.Index

    internal init(_rangeOffset: Int, base: Base.Index) {
      self._rangeOffset = _rangeOffset
      self.base = base
    }
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice.Index: Equatable {
  public static func == (left: Self, right: Self) -> Bool {
    left.base == right.base
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice.Index: Hashable where Base.Index: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(base)
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice.Index: Comparable {
  public static func < (left: Self, right: Self) -> Bool {
    left.base < right.base
  }
}

@available(SwiftStdlib 6.0, *)
@_unavailableInEmbedded
extension DiscontiguousSlice.Index: CustomStringConvertible {
  public var description: String {
    "<base: \(String(reflecting: base)), rangeOffset: \(_rangeOffset)>"
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice.Index: Sendable where Base.Index: Sendable {}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice: Sequence {
  public typealias Element = Base.Element
  public typealias Iterator = IndexingIterator<Self>

  public func _customContainsEquatableElement(_ element: Element) -> Bool? {
    _customIndexOfEquatableElement(element).map { $0 != nil }
  }

  public __consuming func _copyToContiguousArray() -> ContiguousArray<Element> {
    var result: ContiguousArray<Element> = []
    for range in subranges.ranges {
      result.append(contentsOf: base[range])
    }
    return result
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice: Collection {
  public typealias SubSequence = Self
  public typealias Indices = DefaultIndices<Self>
  
  public var startIndex: Index {
    subranges.isEmpty
      ? endIndex
      : Index(_rangeOffset: 0, base: subranges.ranges[0].lowerBound)
  }
  
  public var endIndex: Index {
    Index(_rangeOffset: subranges.ranges.endIndex, base: _base.endIndex)
  }
  
  public var count: Int {
    var c = 0
    for range in subranges.ranges {
      c += base[range].count
    }
    return c
  }

  public var isEmpty: Bool {
    subranges.isEmpty
  }

  public func distance(from start: Index, to end: Index) -> Int {
    let ranges = subranges.ranges[start._rangeOffset ... end._rangeOffset]
    guard ranges.count > 1 else {
      return _base[ranges[0]].distance(from: start.base, to: end.base)
    }
    let firstRange = ranges.first!
    let lastRange = ranges.last!
    let head = _base[firstRange].distance(
      from: start.base, to: firstRange.upperBound)
    let tail = _base[lastRange].distance(
      from: lastRange.lowerBound, to: end.base)
    let middle = ranges[1 ..< (ranges.endIndex - 1)].reduce(0) {
      $0 + _base[$1].count
    }
    return head + middle + tail
  }
  
  public func index(after i: Index) -> Index {
    // Note: index validation performed by the underlying collections only
    let currentRange = subranges.ranges[i._rangeOffset]
    let nextIndex = _base[currentRange].index(after: i.base)
    if nextIndex < currentRange.upperBound {
      return Index(_rangeOffset: i._rangeOffset, base: nextIndex)
    }
    
    let nextOffset = i._rangeOffset + 1
    guard nextOffset < subranges.ranges.endIndex else {
      return endIndex
    }

    return Index(
      _rangeOffset: nextOffset, base: subranges.ranges[nextOffset].lowerBound)
  }

  public subscript(i: Index) -> Base.Element {
    // Note: index validation performed by the base collection only
    _base[subranges.ranges[i._rangeOffset]][i.base]
  }
  
  public subscript(bounds: Range<Index>) -> DiscontiguousSlice<Base> {
    let baseBounds = bounds.lowerBound.base ..< bounds.upperBound.base
    let baseSlice = base[baseBounds]
    let baseAdjustedBounds = baseSlice.startIndex ..< baseSlice.endIndex
    let subset = subranges.intersection(RangeSet(baseAdjustedBounds))
    return DiscontiguousSlice<Base>(_base: base, subranges: subset)
  }

  @usableFromInline
  internal func _index(of baseIndex: Base.Index) -> Index? {
    let rangeOffset = subranges.ranges
      ._partitioningIndex { $0.upperBound >= baseIndex }
    let subrange = subranges.ranges[rangeOffset]
    guard subrange.contains(baseIndex) else {
      return nil
    }
    return Index(_rangeOffset: rangeOffset, base: baseIndex)
  }

  public func _customIndexOfEquatableElement(_ element: Element) -> Index?? {
    var definite = true
    for (i, range) in subranges.ranges.enumerated() {
      guard let baseResult = _base[range]
        ._customIndexOfEquatableElement(element) else {
        definite = false
        continue
      }
      guard let baseIndex = baseResult else {
        continue
      }
      return Index(_rangeOffset: i, base: baseIndex)
    }
    if definite {
      return .some(nil)
    } else {
      return nil
    }
  }

  public func _customLastIndexOfEquatableElement(
    _ element: Element
  ) -> Index?? {
    var definite = true
    for (i, range) in subranges.ranges.enumerated().reversed() {
      guard let baseResult = _base[range]
        ._customLastIndexOfEquatableElement(element) else {
        definite = false
        continue
      }
      guard let baseIndex = baseResult else {
        continue
      }
      return Index(_rangeOffset: i, base: baseIndex)
    }
    if definite {
      return .some(nil)
    } else {
      return nil
    }
  }

  public func _failEarlyRangeCheck(
    _ index: Index, bounds: Range<Index>
  ) {
    let baseBounds = bounds.lowerBound.base ..< bounds.upperBound.base
    let offsetBounds = bounds.lowerBound._rangeOffset ..<
      bounds.upperBound._rangeOffset
    _base._failEarlyRangeCheck(index.base, bounds: baseBounds)
    _precondition(offsetBounds.contains(index._rangeOffset))
    if index._rangeOffset == endIndex._rangeOffset {
      _precondition(index.base == _base.endIndex)
    } else {
      _precondition(subranges.ranges[index._rangeOffset].contains(index.base))
    }
  }

  public func _failEarlyRangeCheck(_ index: Index, bounds: ClosedRange<Index>) {
    let baseBounds = bounds.lowerBound.base ... bounds.upperBound.base
    let offsetBounds = bounds.lowerBound._rangeOffset ...
      bounds.upperBound._rangeOffset
    _base._failEarlyRangeCheck(index.base, bounds: baseBounds)
    _precondition(offsetBounds.contains(index._rangeOffset))
    if index._rangeOffset == endIndex._rangeOffset {
      _precondition(index.base == _base.endIndex)
    } else {
      _precondition(subranges.ranges[index._rangeOffset].contains(index.base))
    }
  }

  public func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>) {
    let baseBounds = bounds.lowerBound.base ..< bounds.upperBound.base
    let baseRange = range.lowerBound.base ..< range.upperBound.base
    let offsetBounds = bounds.lowerBound._rangeOffset ..<
      bounds.upperBound._rangeOffset
    let offsetRange = range.lowerBound._rangeOffset ..<
      range.upperBound._rangeOffset

    _base._failEarlyRangeCheck(baseRange, bounds: baseBounds)
    _precondition(offsetBounds.contains(offsetRange.lowerBound))
    _precondition(offsetRange.upperBound <= offsetBounds.upperBound)

    if offsetRange.lowerBound == endIndex._rangeOffset {
      _precondition(baseRange.lowerBound == _base.endIndex)
    } else {
      _precondition(
        subranges.ranges[offsetRange.lowerBound].contains(baseRange.lowerBound))
    }

    if offsetRange.upperBound == endIndex._rangeOffset {
      _precondition(baseRange.upperBound == _base.endIndex)
    } else {
      _precondition(
        subranges.ranges[offsetRange.upperBound].contains(baseRange.upperBound))
    }
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice: BidirectionalCollection
  where Base: BidirectionalCollection
{
  public func index(before i: Index) -> Index {
    _precondition(i > startIndex, "Can't move index before startIndex")

    if i.base == base.endIndex ||
      i.base == subranges.ranges[i._rangeOffset].lowerBound {
      // Go to next subrange
      let nextOffset = i._rangeOffset - 1
      let nextRange = subranges.ranges[nextOffset]
      let nextBase = base[nextRange].index(before: nextRange.upperBound)
      return Index(_rangeOffset: nextOffset, base: nextBase)
    } else {
      // Move within current subrange
      let currentRange = subranges.ranges[i._rangeOffset]
      let nextBase = base[currentRange].index(before: i.base)
      return Index(_rangeOffset: i._rangeOffset, base: nextBase)
    }
  }
}

@available(SwiftStdlib 6.0, *)
extension DiscontiguousSlice where Base: MutableCollection {
  /// Accesses the element at the specified position.
  ///
  /// For example, you can replace an element of an array by using its
  /// subscript.
  ///
  ///     var streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     streets[1] = "Butler"
  ///     print(streets[1])
  ///     // Prints "Butler"
  ///
  /// You can subscript a collection with any valid index other than the
  /// collection's end index. The end index refers to the position one
  /// past the last element of a collection, so it doesn't correspond with an
  /// element.
  ///
  /// - Parameter position: The position of the element to access. `position`
  ///   must be a valid index of the collection that is not equal to the
  ///   `endIndex` property.
  ///
  /// - Complexity: O(1)
  public subscript(i: Index) -> Base.Element {
    get {
      _base[subranges.ranges[i._rangeOffset]][i.base]
    }
    set {
      _base[subranges.ranges[i._rangeOffset]][i.base] = newValue
    }
  }
}

// MARK: Accessing DiscontiguousSlices

extension Collection {
  /// Accesses a view of this collection with the elements at the given
  /// indices.
  ///
  /// - Parameter subranges: The indices of the elements to retrieve from this
  ///   collection.
  /// - Returns: A collection of the elements at the positions in `subranges`.
  ///
  /// - Complexity: O(1)
  @available(SwiftStdlib 6.0, *)
  public subscript(subranges: RangeSet<Index>) -> DiscontiguousSlice<Self> {
    DiscontiguousSlice(_base: self, subranges: subranges)
  }
}


extension Collection {
  /// Returns a collection of the elements in this collection that are not
  /// represented by the given range set.
  ///
  /// For example, this code sample finds the indices of all the vowel
  /// characters in the string, and then retrieves a collection that omits
  /// those characters.
  ///
  ///     let str = "The rain in Spain stays mainly in the plain."
  ///     let vowels: Set<Character> = ["a", "e", "i", "o", "u"]
  ///     let vowelIndices = str.indices(where: { vowels.contains($0) })
  ///
  ///     let disemvoweled = str.removingSubranges(vowelIndices)
  ///     print(String(disemvoweled))
  ///     // Prints "Th rn n Spn stys mnly n th pln."
  ///
  /// - Parameter subranges: A range set representing the indices of the
  ///   elements to remove.
  /// - Returns: A collection of the elements that are not in `subranges`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the collection.
  @available(SwiftStdlib 6.0, *)
  public func removingSubranges(
    _ subranges: RangeSet<Index>
  ) -> DiscontiguousSlice<Self> {
    let inversion = subranges._inverted(within: self)
    return self[inversion]
  }
}
