//===--- DiscontiguousSlice.swift -----------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A collection wrapper that provides access to the elements of a collection,
/// indexed by a set of indices.
@available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
@frozen
public struct DiscontiguousSlice<Base: Collection> {
  /// The collection that the indexed collection wraps.
  public var base: Base
  
  /// The set of subranges that are available through this discontiguous slice.
  public var subranges: RangeSet<Base.Index>
}

@available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
extension DiscontiguousSlice {
  /// A position in an `DiscontiguousSlice`.
  @frozen
  public struct Index: Comparable {
    /// The index of the range that contains `base`.
    internal var _rangeOffset: Int
    
    /// The position of this index in the base collection.
    public var base: Base.Index
    
    public static func < (lhs: Index, rhs: Index) -> Bool {
      lhs.base < rhs.base
    }
  }
}

@available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
extension DiscontiguousSlice.Index: Hashable where Base.Index: Hashable {}

@available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
extension DiscontiguousSlice: Collection {
  public typealias SubSequence = Self
  
  public var startIndex: Index {
    subranges.isEmpty
      ? endIndex
      : Index(_rangeOffset: 0, base: subranges._ranges[0].lowerBound)
  }
  
  public var endIndex: Index {
    Index(_rangeOffset: subranges._ranges.endIndex, base: base.endIndex)
  }
  
  public func index(after i: Index) -> Index {
    let nextIndex = base.index(after: i.base)
    if subranges._ranges[i._rangeOffset].contains(nextIndex) {
      return Index(_rangeOffset: i._rangeOffset, base: nextIndex)
    }
    
    let nextOffset = i._rangeOffset + 1
    if nextOffset < subranges._ranges.endIndex {
      return Index(
        _rangeOffset: nextOffset,
        base: subranges._ranges[nextOffset].lowerBound)
    } else {
      return endIndex
    }
  }
  
  public subscript(i: Index) -> Base.Element {
    base[i.base]
  }
  
  public subscript(bounds: Range<Index>) -> DiscontiguousSlice<Base> {
    let baseBounds = bounds.lowerBound.base ..< bounds.upperBound.base
    let subset = subranges.intersection(RangeSet(baseBounds))
    return DiscontiguousSlice<Base>(base: base, subranges: subset)
  }
}

@available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
extension DiscontiguousSlice {
  public var count: Int {
    var c = 0
    for range in subranges._ranges {
      c += base.distance(from: range.lowerBound, to: range.upperBound)
    }
    return c
  }
  
  public __consuming func _copyToContiguousArray() -> ContiguousArray<Element> {
    var result: ContiguousArray<Element> = []
    for range in subranges._ranges {
      result.append(contentsOf: base[range])
    }
    return result
  }
}

@available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
extension DiscontiguousSlice: BidirectionalCollection
  where Base: BidirectionalCollection
{
  public func index(before i: Index) -> Index {
    _precondition(i != startIndex, "Can't move index before startIndex")
    
    if i == endIndex || i.base == subranges._ranges[i._rangeOffset].lowerBound {
      let offset = i._rangeOffset - 1
      return Index(
        _rangeOffset: offset,
        base: base.index(before: subranges._ranges[offset].upperBound))
    }
    
    return Index(
      _rangeOffset: i._rangeOffset,
      base: base.index(before: i.base))
  }
}

@available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
extension DiscontiguousSlice: MutableCollection where Base: MutableCollection {
  public subscript(i: Index) -> Base.Element {
    get {
      base[i.base]
    }
    set {
      base[i.base] = newValue
    }
  }
}

// MARK: Subscripts

extension Collection {
  /// Accesses a view of this collection with the elements at the given
  /// indices.
  ///
  /// - Parameter subranges: The indices of the elements to retrieve from this
  ///   collection.
  /// - Returns: A collection of the elements at the positions in `subranges`.
  ///
  /// - Complexity: O(1)
  @available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
  public subscript(subranges: RangeSet<Index>) -> DiscontiguousSlice<Self> {
    DiscontiguousSlice(base: self, subranges: subranges)
  }
}

extension MutableCollection {
  /// Accesses a mutable view of this collection with the elements at the
  /// given indices.
  ///
  /// - Parameter subranges: The ranges of the elements to retrieve from this
  ///   collection.
  /// - Returns: A collection of the elements at the positions in `subranges`.
  ///
  /// - Complexity: O(1) to access the elements, O(*m*) to mutate the
  ///   elements at the positions in `subranges`, where *m* is the number of
  ///   elements indicated by `subranges`.
  @available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
  public subscript(subranges: RangeSet<Index>) -> DiscontiguousSlice<Self> {
    get {
      DiscontiguousSlice(base: self, subranges: subranges)
    }
    set {
      for i in newValue.indices {
        self[i.base] = newValue[i]
      }
    }
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
  ///     let vowelIndices = str.subranges(where: { vowels.contains($0) })
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
  @available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
  public func removingSubranges(
    _ subranges: RangeSet<Index>
  ) -> DiscontiguousSlice<Self> {
    let inversion = subranges._inverted(within: self)
    return self[inversion]
  }
}
