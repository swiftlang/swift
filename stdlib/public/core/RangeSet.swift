/// A set of ranges of any comparable value.
public struct RangeSet<Bound: Comparable> {
  internal var _ranges: [Range<Bound>] = []
  
  /// Creates an empty range set.
  public init() {}
  
  /// Creates a range set containing the given range.
  ///
  /// - Parameter range: The range to use for the new range set.
  public init(_ range: Range<Bound>) {
    if !range.isEmpty {
      self._ranges = [range]
    }
  }
  
  /// Creates a range set containing the given ranges.
  ///
  /// - Parameter ranges: The ranges to use for the new range set.
  public init<S: Sequence>(_ ranges: S) where S.Element == Range<Bound> {
    for range in ranges {
      insert(range)
    }
  }
  
  /// Checks the invariants of `_ranges`.
  ///
  /// The ranges stored by a range set are never empty, never overlap,
  /// and are always stored in ascending order when comparing their lower
  /// or upper bounds. In addition to not overlapping, no two consecutive
  /// ranges share an upper and lower bound — `[0..<5, 5..<10]` is ill-formed,
  /// and would instead be represented as `[0..<10]`.
  internal func _checkInvariants() {
    for (a, b) in zip(ranges, ranges.dropFirst()) {
      precondition(!a.isEmpty && !b.isEmpty, "Empty range in range set")
      precondition(
        a.upperBound < b.lowerBound,
        "Out of order/overlapping ranges in range set")
    }
  }
  
  /// Creates a new range set from `_ranges`, which satisfies the range set
  /// invariants.
  internal init(_ranges: [Range<Bound>]) {
    self._ranges = _ranges
    _checkInvariants()
  }
  
  /// A Boolean value indicating whether the range set is empty.
  public var isEmpty: Bool {
    _ranges.isEmpty
  }
  
  /// Returns a Boolean value indicating whether the given value is
  /// contained by the ranges in the range set.
  ///
  /// - Parameter value: The value to look for in the range set.
  /// - Return: `true` if `value` is contained by a range in the range set;
  ///   otherwise, `false`.
  ///
  /// - Complexity: O(log *n*), where *n* is the number of ranges in the
  ///   range set.
  public func contains(_ value: Bound) -> Bool {
    let i = _ranges._partitioningIndex { $0.upperBound > value }
    return i == _ranges.endIndex
      ? false
      : _ranges[i].lowerBound <= value
  }
  
  /// Returns a Boolean value indicating whether the given range is
  /// contained in the range set.
  ///
  /// - Parameter range: The range to look for in the range set.
  /// - Return: `true` if `element` is contained in the range set; otherwise,
  ///   `false`.
  ///
  /// - Complexity: O(log *n*), where *n* is the number of ranges in the
  ///   range set.
  public func contains(_ range: Range<Bound>) -> Bool {
    let i = _ranges._partitioningIndex { $0.upperBound >= range.upperBound }
    return i == _ranges.endIndex
      ? false
      : _ranges[i].lowerBound <= range.lowerBound
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
  func _indicesOfRange(_ range: Range<Bound>) -> Range<Int> {
    precondition(!range.isEmpty)
    precondition(!_ranges.isEmpty)
    precondition(range.lowerBound <= _ranges.last!.upperBound)
    precondition(range.upperBound >= _ranges.first!.lowerBound)
    
    // The beginning index for the position of `range` is the first range
    // with an upper bound larger than `range`'s lower bound. The range
    // at this position may or may not overlap `range`.
    let beginningIndex = _ranges
      ._partitioningIndex { $0.upperBound >= range.lowerBound }
    
    // The ending index for `range` is the first range with a lower bound
    // greater than `range`'s upper bound. If this is the same as
    // `beginningIndex`, than `range` doesn't overlap any of the existing
    // ranges. If this is `ranges.endIndex`, then `range` overlaps the
    // rest of the ranges. Otherwise, `range` overlaps one or
    // more ranges in the set.
    let endingIndex = _ranges[beginningIndex...]
      ._partitioningIndex { $0.lowerBound > range.upperBound }
    
    return beginningIndex ..< endingIndex
  }
  
  /// Inserts a non-empty range that is known to be greater than all the 
  /// elements in the set so far.
  ///
  /// - Precondition: The range set must be empty, or else
  ///   `ranges.last!.upperBound <= range.lowerBound`.
  /// - Precondition: `range` must not be empty.
  internal mutating func _append(_ range: Range<Bound>) {
    precondition(_ranges.isEmpty
      || _ranges.last!.upperBound <= range.lowerBound)
    precondition(!range.isEmpty)
    if _ranges.isEmpty {
      _ranges.append(range)
    } else if _ranges.last!.upperBound == range.lowerBound {
      _ranges[_ranges.count - 1] =
        _ranges[_ranges.count - 1].lowerBound ..< range.upperBound
    } else {
      _ranges.append(range)
    }
  }
  
  /// Inserts the given range into the range set.
  ///
  /// - Parameter range: The range to insert into the set.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func insert(_ range: Range<Bound>) {
    // Shortcuts for the (literal) edge cases
    if range.isEmpty { return }
    guard !_ranges.isEmpty else {
      _ranges.append(range)
      return
    }
    guard range.lowerBound < _ranges.last!.upperBound else {
      _append(range)
      return
    }
    guard range.upperBound >= _ranges.first!.lowerBound else {
      _ranges.insert(range, at: 0)
      return
    }
    
    let indices = _indicesOfRange(range)
    
    // Non-overlapping is a simple insertion.
    guard !indices.isEmpty else {
      _ranges.insert(range, at: indices.lowerBound)
      return
    }
    
    // Find the lower and upper bounds of the overlapping ranges.
    let newLowerBound = Swift.min(
      _ranges[indices.lowerBound].lowerBound,
      range.lowerBound)
    let newUpperBound = Swift.max(
      _ranges[indices.upperBound - 1].upperBound,
      range.upperBound)
    _ranges.replaceSubrange(
      indices,
      with: CollectionOfOne(newLowerBound..<newUpperBound))
  }
  
  /// Removes the given range from the range set.
  ///
  /// - Parameter range: The range to remove from the set.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func remove(_ range: Range<Bound>) {
    // Shortcuts for the (literal) edge cases
    if range.isEmpty
      || _ranges.isEmpty
      || range.lowerBound >= _ranges.last!.upperBound
      || range.upperBound < _ranges.first!.lowerBound
    { return }
    
    let indices = _indicesOfRange(range)
    
    // No actual overlap, nothing to remove.
    if indices.isEmpty { return }
    
    let overlapsLowerBound =
      range.lowerBound > _ranges[indices.lowerBound].lowerBound
    let overlapsUpperBound =
      range.upperBound < _ranges[indices.upperBound - 1].upperBound
    
    switch (overlapsLowerBound, overlapsUpperBound) {
    case (false, false):
      _ranges.removeSubrange(indices)
    case (false, true):
      let newRange =
        range.upperBound..<_ranges[indices.upperBound - 1].upperBound
      _ranges.replaceSubrange(indices, with: CollectionOfOne(newRange))
    case (true, false):
      let newRange = _ranges[indices.lowerBound].lowerBound..<range.lowerBound
      _ranges.replaceSubrange(indices, with: CollectionOfOne(newRange))
    case (true, true):
      _ranges.replaceSubrange(indices, with: Pair(
        _ranges[indices.lowerBound].lowerBound..<range.lowerBound,
        range.upperBound..<_ranges[indices.upperBound - 1].upperBound
      ))
    }
  }
}

extension RangeSet: Equatable {}

extension RangeSet: Hashable where Bound: Hashable {}

extension RangeSet: ExpressibleByArrayLiteral {
  public init(arrayLiteral elements: Range<Bound>...) {
    self.init(elements)
  }
}

// MARK: - Range Collection

extension RangeSet {
  public struct Ranges: RandomAccessCollection {
    var _ranges: [Range<Bound>]
    
    public var startIndex: Int { _ranges.startIndex }
    public var endIndex: Int { _ranges.endIndex }
    
    public subscript(i: Int) -> Range<Bound> {
      _ranges[i]
    }
  }
  
  /// A collection of the ranges that make up the range set.
  public var ranges: Ranges {
    Ranges(_ranges: _ranges)
  }
}

// MARK: - Collection APIs

extension RangeSet {
  /// Creates a new range set containing a range that contains only the
  /// specified index in the given collection.
  ///
  /// - Parameters:
  ///   - index: The index to include in the range set. `index` must be a
  ///     valid index of `collection` that isn't the collection's `endIndex`.
  ///   - collection: The collection that contains `index`.
  public init<C>(_ index: Bound, within collection: C)
    where C: Collection, C.Index == Bound
  {
    let range = index..<collection.index(after: index)
    self.init(range)
  }
  
  /// Creates a new range set containing ranges that contain only the
  /// specified indices in the given collection.
  ///
  /// - Parameters:
  ///   - index: The index to include in the range set. `index` must be a
  ///     valid index of `collection` that isn't the collection's `endIndex`.
  ///   - collection: The collection that contains `index`.
  public init<S, C>(_ indices: S, within collection: C)
    where S: Sequence, C: Collection, S.Element == C.Index, C.Index == Bound
  {
    for i in indices {
      self.insert(i, within: collection)
    }
  }
  
  /// Creates a new range set containing the range represented by the
  /// specified range expression.
  ///
  /// - Parameters:
  ///   - range: The range expression to use as the set's initial range.
  ///   - collection: The collection that `range` is relative to.
  public init<R, C>(_ range: R, within collection: C)
    where C: Collection, C.Index == Bound, R: RangeExpression, R.Bound == Bound
  {
    let concreteRange = range.relative(to: collection)
    self.init(concreteRange)
  }
  
  /// Inserts a range that contains only the specified index into the range
  /// set.
  ///
  /// - Parameters:
  ///   - index: The index to insert into the range set. `index` must be a
  ///     valid index of `collection` that isn't the collection's `endIndex`.
  ///   - collection: The collection that contains `index`.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func insert<C>(_ index: Bound, within collection: C)
    where C: Collection, C.Index == Bound
  {
    insert(index ..< collection.index(after: index))
  }
  
  /// Inserts the range represented by the specified range expression into
  /// the range set.
  ///
  /// - Parameters:
  ///   - range: The range expression to insert into the range set.
  ///   - collection: The collection that `range` is relative to.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func insert<R, C>(_ range: R, within collection: C)
    where C: Collection, C.Index == Bound, R: RangeExpression, R.Bound == Bound
  {
    let concreteRange = range.relative(to: collection)
    insert(concreteRange)
  }
  
  /// Removes the range that contains only the specified index from the range
  /// set.
  ///
  /// - Parameters:
  ///   - index: The index to remove from the range set. `index` must be a
  ///     valid index of `collection` that isn't the collection's `endIndex`.
  ///   - collection: The collection that contains `index`.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func remove<C>(_ index: Bound, within collection: C)
    where C: Collection, C.Index == Bound
  {
    remove(index ..< collection.index(after: index))
  }
  
  /// Removes the range represented by the specified range expression from
  /// the range set.
  ///
  /// - Parameters:
  ///   - range: The range expression to remove from the range set.
  ///   - collection: The collection that `range` is relative to.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public mutating func remove<R, C>(_ range: R, within collection: C)
    where C: Collection, C.Index == Bound, R: RangeExpression, R.Bound == Bound
  {
    let concreteRange = range.relative(to: collection)
    remove(concreteRange)
  }
  
  /// Returns a range set that represents all the elements in the given
  /// collection that aren't represented by this range set.
  ///
  /// The following example finds the indices of the vowels in a string, and
  /// then inverts the range set to find the non-vowels parts of the string.
  ///
  ///     let str = "The rain in Spain stays mainly in the plain."
  ///     let vowels = "aeiou"
  ///     let vowelIndices = str.indices(where: { vowels.contains($0) })
  ///     print(String(str[vowelIndices]))
  ///     // Prints "eaiiaiaaiieai"
  ///
  ///     let nonVowelIndices = vowelIndices.inverted(within: str)
  ///     print(String(str[nonVowelIndices]))
  ///     // Prints "Th rn n Spn stys mnly n th pln."
  ///
  /// - Parameter collection: The collection that the range set is relative
  ///   to.
  /// - Returns: A new range set that represents the elements in `collection`
  ///   that aren't represented by this range set.
  ///
  /// - Complexity: O(*n*), where *n* is the number of ranges in the range
  ///   set.
  public func inverted<C>(within collection: C) -> RangeSet
    where C: Collection, C.Index == Bound
  {
    var result: RangeSet = []
    var low = collection.startIndex
    for range in _ranges {
      result.insert(low..<range.lowerBound)
      low = range.upperBound
    }
    result.insert(low..<collection.endIndex)
    return result
  }
}

// MARK: - SetAlgebra

// These methods only depend on the ranges that comprise the range set, so
// we can provide them even when we can't provide `SetAlgebra` conformance.
extension RangeSet {
  public mutating func formUnion(_ other: __owned RangeSet<Bound>) {
    for range in other._ranges {
      insert(range)
    }
  }
  
  public mutating func formIntersection(_ other: RangeSet<Bound>) {
    self = self.intersection(other)
  }
  
  public mutating func formSymmetricDifference(
    _ other: __owned RangeSet<Bound>
  ) {
    self = self.symmetricDifference(other)
  }
  
  public mutating func subtract(_ other: RangeSet<Bound>) {
    for range in other._ranges {
      remove(range)
    }
  }
  
  public __consuming func union(
    _ other: __owned RangeSet<Bound>
  ) -> RangeSet<Bound> {
    var result = self
    result.formUnion(other)
    return result
  }
  
  public __consuming func intersection(
    _ other: RangeSet<Bound>
  ) -> RangeSet<Bound> {
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
    
    for currentRange in _ranges {
      // Search forward in `other` until finding either an overlapping
      // range or one that is strictly higher than this range.
      while otherRangeIndex < other._ranges.endIndex &&
        other._ranges[otherRangeIndex].upperBound <= currentRange.lowerBound
      {
        otherRangeIndex += 1
      }
      
      // For each range in `other` that overlaps with the current range
      // in `self`, append the intersection to the result.
      while otherRangeIndex < other._ranges.endIndex &&
        other._ranges[otherRangeIndex].lowerBound < currentRange.upperBound
      {
        let lower = Swift.max(
          other._ranges[otherRangeIndex].lowerBound,
          currentRange.lowerBound)
        let upper = Swift.min(
          other._ranges[otherRangeIndex].upperBound,
          currentRange.upperBound)
        result.append(lower..<upper)
        
        // If the range in `other` continues past the current range in
        // `self`, it could overlap the next range in `self`, so break
        // out of examining the current range.
        guard
          currentRange.upperBound > other._ranges[otherRangeIndex].upperBound
        else {
            break
        }
        otherRangeIndex += 1
      }
    }
    
    return RangeSet(_ranges: result)
  }
  
  public __consuming func symmetricDifference(
    _ other: __owned RangeSet<Bound>
  ) -> RangeSet<Bound> {
    return union(other).subtracting(intersection(other))
  }
  
  public func subtracting(_ other: RangeSet<Bound>) -> RangeSet<Bound> {
    var result = self
    result.subtract(other)
    return result
  }
  
  public func isSubset(of other: RangeSet<Bound>) -> Bool {
    self.intersection(other) == self
  }
  
  public func isSuperset(of other: RangeSet<Bound>) -> Bool {
    other.isSubset(of: self)
  }
  
  public func isStrictSubset(of other: RangeSet<Bound>) -> Bool {
    self != other && isSubset(of: other)
  }
  
  public func isStrictSuperset(of other: RangeSet<Bound>) -> Bool {
    other.isStrictSubset(of: self)
  }
}

extension RangeSet: CustomStringConvertible {
  public var description: String {
    let rangesDescription = _ranges
      .map { r in "\(r.lowerBound)..<\(r.upperBound)" }
      .joined(separator: ", ")
    return "RangeSet(\(rangesDescription))"
  }
}

/// A collection of two elements, to avoid heap allocation when calling
/// `replaceSubrange` with just two elements.
internal struct Pair<Element>: RandomAccessCollection {
  var pair: (first: Element, second: Element)
  
  init(_ first: Element, _ second: Element) {
    self.pair = (first, second)
  }
  
  var startIndex: Int { 0 }
  var endIndex: Int { 2 }
  
  subscript(position: Int) -> Element {
    get {
      switch position {
      case 0: return pair.first
      case 1: return pair.second
      default: fatalError("Index '\(position)' is out of range")
      }
    }
  }
}
