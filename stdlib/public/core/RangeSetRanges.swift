//===--- RangeSetRanges.swift ---------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.11, *)
extension RangeSet {
  /// A collection of the ranges that make up a range set.
  public struct Ranges {
    internal var _storage: ContiguousArray<Range<Bound>>

    @usableFromInline
    internal init() {
      _storage = []
    }
    
    @usableFromInline
    internal init(_range: Range<Bound>) {
      _storage = [_range]
    }
    
    @usableFromInline
    internal init(_ranges: [Range<Bound>]) {
      _storage = ContiguousArray(_ranges)
    }

    @usableFromInline
    internal init(_unorderedRanges: [Range<Bound>]) {
      _storage = ContiguousArray(_unorderedRanges)
      _storage.sort {
        $0.lowerBound < $1.lowerBound
      }
      var i = 0
      while i < _storage.count {
        let current = _storage[i]
        if i > 0 {
          let previous = _storage[i - 1]
          if previous.upperBound >= current.lowerBound {
            let newUpper = Swift.max(previous.upperBound, current.upperBound)
            _storage[i - 1] = previous.lowerBound ..< newUpper
            _storage.remove(at: i)
            continue
          } 
        }

        if current.isEmpty {
          _storage.remove(at: i)
        } else {
          i += 1
        }
      }
    }
  }
}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges {
  @usableFromInline
  internal func _contains(_ bound: Bound) -> Bool {
    let i = _storage._partitioningIndex { $0.upperBound > bound }
    return i == _storage.endIndex ? false : _storage[i].lowerBound <= bound
  }

  /// Returns a range indicating the existing ranges that `range` overlaps
  /// with.
  ///
  /// For example, if `self` is `[0..<5, 10..<15, 20..<25, 30..<35]`, then:
  ///
  /// - `_indicesOfRange(12..<14) == 1..<2`
  /// - `_indicesOfRange(12..<19) == 1..<2`
  /// - `_indicesOfRange(17..<19) == 2..<2`
  /// - `_indicesOfRange(12..<22) == 1..<3`
  @usableFromInline
  internal func _indicesOfRange(
    _ range: Range<Bound>,
    in subranges: ContiguousArray<Range<Bound>>,
    includeAdjacent: Bool = true
  ) -> Range<Int> {
    guard subranges.count > 1 else {
      if subranges.isEmpty {
        return 0 ..< 0
      } else {
        let subrange = subranges[0]
        if range.upperBound < subrange.lowerBound {
          return 0 ..< 0
        } else if range.lowerBound > subrange.upperBound {
          return 1 ..< 1
        } else {
          return 0 ..< 1
        }
      }
    }

    // The beginning index for the position of `range` is the first range
    // with an upper bound larger than `range`'s lower bound. The range
    // at this position may or may not overlap `range`.
    let beginningIndex = subranges._partitioningIndex {
      if includeAdjacent {
        $0.upperBound >= range.lowerBound
      } else {
        $0.upperBound > range.lowerBound
      }
    }
    
    // The ending index for `range` is the first range with a lower bound
    // greater than `range`'s upper bound. If this is the same as
    // `beginningIndex`, than `range` doesn't overlap any of the existing
    // ranges. If this is `ranges.endIndex`, then `range` overlaps the
    // rest of the ranges. Otherwise, `range` overlaps one or
    // more ranges in the set.
    let endingIndex = subranges[beginningIndex...]._partitioningIndex {
      if includeAdjacent {
        $0.lowerBound > range.upperBound
      } else {
        $0.lowerBound >= range.upperBound
      }
    }
    
    return beginningIndex ..< endingIndex
  }

  // Insert a non-empty range into the storage
  @usableFromInline
  @discardableResult
  internal mutating func _insert(contentsOf range: Range<Bound>) -> Bool {
  	let indices = _indicesOfRange(range, in: _storage)
  	if indices.isEmpty {
  		_storage.insert(range, at: indices.lowerBound)
  		return true
  	} else {
  		let lower = Swift.min(
  			_storage[indices.lowerBound].lowerBound, range.lowerBound)
  		let upper = Swift.max(
  			_storage[indices.upperBound - 1].upperBound, range.upperBound)
  		let newRange = lower ..< upper
  		if indices.count == 1 && newRange == _storage[indices.lowerBound] {
  			return false
  		}
  		_storage.replaceSubrange(indices, with: CollectionOfOne(newRange))
  		return true
  	}
  }

  // Remove a non-empty range from the storage
  @usableFromInline
  internal mutating func _remove(contentsOf range: Range<Bound>) {
    let indices = _indicesOfRange(range, in: _storage, includeAdjacent: false)
    guard !indices.isEmpty else {
      return
    }
    
    let overlapsLowerBound =
      range.lowerBound > _storage[indices.lowerBound].lowerBound
    let overlapsUpperBound =
      range.upperBound < _storage[indices.upperBound - 1].upperBound

    switch (overlapsLowerBound, overlapsUpperBound) {
    case (false, false):
      _storage.removeSubrange(indices)
    case (false, true):
      let newRange =
        range.upperBound..<_storage[indices.upperBound - 1].upperBound
      _storage.replaceSubrange(indices, with: CollectionOfOne(newRange))
    case (true, false):
      let newRange =
        _storage[indices.lowerBound].lowerBound ..< range.lowerBound
      _storage.replaceSubrange(indices, with: CollectionOfOne(newRange))
    case (true, true):
      _storage.replaceSubrange(indices, with: _Pair(
        _storage[indices.lowerBound].lowerBound..<range.lowerBound,
        range.upperBound..<_storage[indices.upperBound - 1].upperBound
      ))
    }
  }

  /// Returns a that represents the ranges of values within the
  /// given bounds that aren't represented by this range set.
  @usableFromInline
  internal func _gaps(boundedBy bounds: Range<Bound>) -> Self {
    let indices = _indicesOfRange(bounds, in: _storage)
    guard !indices.isEmpty else {
      return Self(_range: bounds)
    }
    
    var result: [Range<Bound>] = []
    var low = bounds.lowerBound
    for range in _storage[indices] {
      let gapRange = low ..< range.lowerBound
      if !gapRange.isEmpty {
        result.append(gapRange)
      }
      low = range.upperBound
    }
    let finalRange = low ..< bounds.upperBound
    if !finalRange.isEmpty {
      result.append(finalRange)
    }
    let resultVal = Self(_ranges: result)
    return resultVal
  }

  @usableFromInline
  internal func _intersection(_ other: Self) -> Self {
    let left = self._storage
    let right = other._storage
    var otherRangeIndex = 0
    var result: [Range<Bound>] = []
    
    // Considering these two range sets:
    //
    //     self = [0..<5, 9..<14]
    //     other = [1..<3, 4..<6, 8..<12]
    //
    // `self.intersection(other)` looks like this, where x's cover the
    // ranges in `self`, y's cover the ranges in `other`, and z's cover the
    // resulting ranges:
    //
    //   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
    //   xxxxxxxxxxxxxxxxxxx__               xxxxxxxxxxxxxxxxxxx__
    //       yyyyyyy__   yyyyyyy__       yyyyyyyyyyyyyyy__
    //       zzzzzzz__   zzz__               zzzzzzzzzzz__
    //
    // The same, but for `other.intersection(self)`:
    //
    //   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15
    //       xxxxxxx__   xxxxxxx__       xxxxxxxxxxxxxxx__
    //   yyyyyyyyyyyyyyyyyyy__               yyyyyyyyyyyyyyyyyyy__
    //       zzzzzzz__   zzz__               zzzzzzzzzzz__
    
    for currentRange in left {
      // Search forward in `right` until finding either an overlapping
      // range or one that is strictly higher than this range.
      while otherRangeIndex < right.endIndex &&
        right[otherRangeIndex].upperBound <= currentRange.lowerBound
      {
        otherRangeIndex += 1
      }
      
      // For each range in `right` that overlaps with the current range
      // in `left`, append the intersection to the result.
      while otherRangeIndex < right.endIndex &&
        right[otherRangeIndex].lowerBound < currentRange.upperBound
      {
        let overlap = right[otherRangeIndex].clamped(to: currentRange)
        result.append(overlap)
        
        // If the range in `right` continues past the current range in
        // `self`, it could overlap the next range in `self`, so break
        // out of examining the current range.
        guard
          currentRange.upperBound > right[otherRangeIndex].upperBound
        else {
          break
        }
        otherRangeIndex += 1
      }
    }
    
    return Self(_ranges: result)
  }
}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges: Sequence {
  public typealias Element = Range<Bound>
  public typealias Iterator = IndexingIterator<Self>
}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges: Collection {
  public typealias Index = Int
  public typealias Indices = Range<Index>
  public typealias SubSequence = Slice<Self>
  
  public var startIndex: Index {
    0
  }
  
  public var endIndex: Index {
    _storage.count
  }
  
  public var count: Int {
    self.endIndex
  }

  public subscript(i: Index) -> Element {
    _storage[i]
  }
}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges: RandomAccessCollection {}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges: Equatable {
  public static func == (left: Self, right: Self) -> Bool {
    left._storage == right._storage
  }
}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges: Hashable where Bound: Hashable {
  public func hash(into hasher: inout Hasher) {
    hasher.combine(_storage)
  }
}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges: Sendable where Bound: Sendable {}

@available(SwiftStdlib 5.11, *)
extension RangeSet.Ranges: CustomStringConvertible {
  public var description: String {
    _makeCollectionDescription()
  }
}

/// A collection of two elements, to avoid heap allocation when calling
/// `replaceSubrange` with just two elements.
internal struct _Pair<Element>: RandomAccessCollection {
  internal var pair: (first: Element, second: Element)
  
  internal init(_ first: Element, _ second: Element) {
    self.pair = (first, second)
  }
  
  internal var startIndex: Int { 0 }
  internal var endIndex: Int { 2 }
  
  internal subscript(position: Int) -> Element {
    get {
      switch position {
      case 0: return pair.first
      case 1: return pair.second
      default: _preconditionFailure("Index is out of range")
      }
    }
  }
}