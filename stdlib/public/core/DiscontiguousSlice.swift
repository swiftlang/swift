/// A collection wrapper that provides access to the elements of a collection,
/// indexed by a set of indices.
public struct DiscontiguousSlice<Base: Collection> {
  /// The collection that the indexed collection wraps.
  public var base: Base
  
  /// The set of index ranges that are available through this indexing
  /// collection.
  public var ranges: RangeSet<Base.Index>
}

extension DiscontiguousSlice {
  /// A position in an `IndexingCollection`.
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

extension DiscontiguousSlice.Index: Hashable where Base.Index: Hashable {}

extension DiscontiguousSlice: Collection {
  public typealias SubSequence = Self
  
  public var startIndex: Index {
    ranges.isEmpty
      ? endIndex
      : Index(_rangeOffset: 0, base: ranges._ranges[0].lowerBound)
  }
  
  public var endIndex: Index {
    Index(_rangeOffset: ranges._ranges.endIndex, base: base.endIndex)
  }
  
  public func index(after i: Index) -> Index {
    let nextIndex = base.index(after: i.base)
    if ranges._ranges[i._rangeOffset].contains(nextIndex) {
      return Index(_rangeOffset: i._rangeOffset, base: nextIndex)
    }
    
    let nextOffset = i._rangeOffset + 1
    if nextOffset < ranges._ranges.endIndex {
      return Index(
        _rangeOffset: nextOffset,
        base: ranges._ranges[nextOffset].lowerBound)
    } else {
      return endIndex
    }
  }
  
  public subscript(i: Index) -> Base.Element {
    base[i.base]
  }
  
  public subscript(bounds: Range<Index>) -> DiscontiguousSlice<Base> {
    let baseBounds = bounds.lowerBound.base ..< bounds.upperBound.base
    let subset = ranges.intersection(RangeSet(baseBounds))
    return DiscontiguousSlice<Base>(base: base, ranges: subset)
  }
}

extension DiscontiguousSlice {
  public var count: Int {
    var c = 0
    for range in ranges._ranges {
      c += base.distance(from: range.lowerBound, to: range.upperBound)
    }
    return c
  }
  
  public __consuming func _copyToContiguousArray() -> ContiguousArray<Element> {
    var result: ContiguousArray<Element> = []
    for range in ranges._ranges {
      result.append(contentsOf: base[range])
    }
    return result
  }
}

extension DiscontiguousSlice: BidirectionalCollection
  where Base: BidirectionalCollection
{
  public func index(before i: Index) -> Index {
    precondition(i != startIndex, "Can't move index before startIndex")
    
    if i == endIndex || i.base == ranges._ranges[i._rangeOffset].lowerBound {
      let offset = i._rangeOffset - 1
      return Index(
        _rangeOffset: offset,
        base: base.index(before: ranges._ranges[offset].upperBound))
    }
    
    return Index(
      _rangeOffset: i._rangeOffset,
      base: base.index(before: i.base))
  }
}

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
  /// - Parameter indices: The indices of the elements to retrieve from this
  ///   collection.
  /// - Returns: A collection of the elements at the positions in `indices`.
  ///
  /// - Complexity: O(1)
  public subscript(indices: RangeSet<Index>) -> DiscontiguousSlice<Self> {
    DiscontiguousSlice(base: self, ranges: indices)
  }
}

extension MutableCollection {
  /// Accesses a mutable view of this collection with the elements at the
  /// given indices.
  ///
  /// - Parameter indices: The indices of the elements to retrieve from this
  ///   collection.
  /// - Returns: A collection of the elements at the positions in `indices`.
  ///
  /// - Complexity: O(1) to access the elements, O(*m*) to mutate the
  ///   elements at the positions in `indices`, where *m* is the number of
  ///   elements indicated by `indices`.
  public subscript(indices: RangeSet<Index>) -> DiscontiguousSlice<Self> {
    get {
      DiscontiguousSlice(base: self, ranges: indices)
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
  ///     let vowelIndices = str.indices(where: { vowels.contains($0) })
  ///
  ///     let disemvoweled = str.removingAll(at: vowelIndices)
  ///     print(String(disemvoweled))
  ///     // Prints "Th rn n Spn stys mnly n th pln."
  ///
  /// - Parameter indices: A range set representing the elements to remove.
  /// - Returns: A collection of the elements that are not in `indices`.
  ///
  /// - Complexity: O(*n*), where *n* is the length of the collection.
  public func removingAll(
    at indices: RangeSet<Index>
  ) -> DiscontiguousSlice<Self> {
    let inversion = indices.inverted(within: self)
    return self[inversion]
  }
}
