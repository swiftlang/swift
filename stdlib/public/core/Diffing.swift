//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2015 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// MARK: Diff application to RangeReplaceableCollection

@available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) // FIXME(availability-5.1)
extension CollectionDifference {
  fileprivate func _fastEnumeratedApply(
    _ consume: (Change) -> Void
  ) {
    let totalRemoves = removals.count
    let totalInserts = insertions.count
    var enumeratedRemoves = 0
    var enumeratedInserts = 0

    while enumeratedRemoves < totalRemoves || enumeratedInserts < totalInserts {
      let change: Change
      if enumeratedRemoves < removals.count && enumeratedInserts < insertions.count {
        let removeOffset = removals[enumeratedRemoves]._offset
        let insertOffset = insertions[enumeratedInserts]._offset
        if removeOffset - enumeratedRemoves <= insertOffset - enumeratedInserts {
          change = removals[enumeratedRemoves]
        } else {
          change = insertions[enumeratedInserts]
        }
      } else if enumeratedRemoves < totalRemoves {
        change = removals[enumeratedRemoves]
      } else if enumeratedInserts < totalInserts {
        change = insertions[enumeratedInserts]
      } else {
        // Not reached, loop should have exited.
        preconditionFailure()
      }

      consume(change)

      switch change {
      case .remove(_, _, _):
        enumeratedRemoves += 1
      case .insert(_, _, _):
        enumeratedInserts += 1
      }
    }
  }
}

extension RangeReplaceableCollection {
  /// Applies the given difference to this collection.
  ///
  /// - Parameter difference: The difference to be applied.
  ///
  /// - Returns: An instance representing the state of the receiver with the
  ///   difference applied, or `nil` if the difference is incompatible with
  ///   the receiver's state.
  ///
  /// - Complexity: O(*n* + *c*), where *n* is `self.count` and *c* is the
  ///   number of changes contained by the parameter.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) // FIXME(availability-5.1)
  public func applying(_ difference: CollectionDifference<Element>) -> Self? {
    var result = Self()
    var enumeratedRemoves = 0
    var enumeratedInserts = 0
    var enumeratedOriginals = 0
    var currentIndex = self.startIndex

    func append(
      into target: inout Self,
      contentsOf source: Self,
      from index: inout Self.Index, count: Int
    ) {
      let start = index
      source.formIndex(&index, offsetBy: count)
      target.append(contentsOf: source[start..<index])
    }

    difference._fastEnumeratedApply { change in
      switch change {
      case .remove(offset: let offset, element: _, associatedWith: _):
        let origCount = offset - enumeratedOriginals
        append(into: &result, contentsOf: self, from: &currentIndex, count: origCount)
        enumeratedOriginals += origCount + 1 // Removal consumes an original element
        currentIndex = self.index(after: currentIndex)
        enumeratedRemoves += 1
      case .insert(offset: let offset, element: let element, associatedWith: _):
        let origCount = (offset + enumeratedRemoves - enumeratedInserts) - enumeratedOriginals
        append(into: &result, contentsOf: self, from: &currentIndex, count: origCount)
        result.append(element)
        enumeratedOriginals += origCount
        enumeratedInserts += 1
      }
      _internalInvariant(enumeratedOriginals <= self.count)
    }
    let origCount = self.count - enumeratedOriginals
    append(into: &result, contentsOf: self, from: &currentIndex, count: origCount)

    _internalInvariant(currentIndex == self.endIndex)
    _internalInvariant(enumeratedOriginals + origCount == self.count)
    _internalInvariant(result.count == self.count + enumeratedInserts - enumeratedRemoves)
    return result
  }
}

// MARK: Definition of API

extension BidirectionalCollection {
  /// Returns the difference needed to produce this collection's ordered 
  /// elements from the given collection, using the given predicate as an 
  /// equivalence test.
  ///
  /// This function does not infer element moves. If you need to infer moves,
  /// call the `inferringMoves()` method on the resulting difference.
  ///
  /// - Parameters:
  ///   - other: The base state.
  ///   - areEquivalent: A closure that returns a Boolean value indicating 
  ///     whether two elements are equivalent.
  ///
  /// - Returns: The difference needed to produce the reciever's state from
  ///   the parameter's state.
  ///
  /// - Complexity: Worst case performance is O(*n* * *m*), where *n* is the 
  ///   count of this collection and *m* is `other.count`. You can expect 
  ///   faster execution when the collections share many common elements.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) // FIXME(availability-5.1)
  public func difference<C: BidirectionalCollection>(
    from other: C,
    by areEquivalent: (Element, C.Element) -> Bool
  ) -> CollectionDifference<Element>
  where C.Element == Self.Element {
    var rawChanges: [CollectionDifference<Element>.Change] = []

    let source = _CountingIndexCollection(other)
    let target = _CountingIndexCollection(self)
    let result = _CollectionChanges(from: source, to: target, by: areEquivalent)
    for change in result {
      switch change {
      case let .removed(r):
        for i in source.indices[r] {
          rawChanges.append(
            .remove(
              offset: i.offset!,
              element: source[i],
              associatedWith: nil))
        }
      case let .inserted(r):
        for i in target.indices[r] {
          rawChanges.append(
            .insert(
              offset: i.offset!,
              element: target[i],
              associatedWith: nil))
        }
      case .matched: break
      }
    }

    return CollectionDifference<Element>(_validatedChanges: rawChanges)
  }
}

extension BidirectionalCollection where Element : Equatable {
  /// Returns the difference needed to produce this collection's ordered 
  /// elements from the given collection.
  ///
  /// This function does not infer element moves. If you need to infer moves,
  /// call the `inferringMoves()` method on the resulting difference.
  ///
  /// - Parameters:
  ///   - other: The base state.
  ///
  /// - Returns: The difference needed to produce this collection's ordered 
  ///   elements from the given collection.
  ///
  /// - Complexity: Worst case performance is O(*n* * *m*), where *n* is the 
  ///   count of this collection and *m* is `other.count`. You can expect 
  ///   faster execution when the collections share many common elements, or 
  ///   if `Element` conforms to `Hashable`.
  @available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) // FIXME(availability-5.1)
  public func difference<C: BidirectionalCollection>(
    from other: C
  ) -> CollectionDifference<Element> where C.Element == Self.Element {
    return difference(from: other, by: ==)
  }
}

extension BidirectionalCollection {
  /// Returns a pair of subsequences containing the initial elements that
  /// `self` and `other` have in common.
  fileprivate func _commonPrefix<Other: BidirectionalCollection>(
    with other: Other,
    by areEquivalent: (Element, Other.Element) -> Bool
  ) -> (SubSequence, Other.SubSequence)
  where Element == Other.Element {
    let (s1, s2) = (startIndex, other.startIndex)
    let (e1, e2) = (endIndex, other.endIndex)
    var (i1, i2) = (s1, s2)
    while i1 != e1 && i2 != e2 {
      if !areEquivalent(self[i1], other[i2]) { break }
      formIndex(after: &i1)
      other.formIndex(after: &i2)
    }
    return (self[s1..<i1], other[s2..<i2])
  }
}

// MARK: Diff production from OrderedCollection (soon to be BidirectionalCollection)

/// A collection of changes between a source and target collection.
///
/// It can be used to traverse the [longest common subsequence][lcs] of
/// source and target:
///
///   let changes = CollectionChanges(from: source, to target)
///   for case let .match(s, t) in changes {
///     // use `s`, `t`
///   }
///
/// It can also be used to traverse the [shortest edit script][ses] of
/// remove and insert operations:
///
///   let changes = CollectionChanges(from: source, to target)
///   for c in changes {
///     switch c {
///     case let .removed(s):
///       // use `s`
///     case let .inserted(t):
///       // use `t`
///     case .matched: continue
///     }
///   }
///
/// [lcs]: http://en.wikipedia.org/wiki/Longest_common_subsequence_problem
/// [ses]: http://en.wikipedia.org/wiki/Edit_distance
///
/// - Note: `CollectionChanges` holds a reference to state used to run the
///     difference algorithm, which can be exponentially larger than the
///     changes themselves.
fileprivate struct _CollectionChanges<SourceIndex, TargetIndex>
where SourceIndex: Comparable, TargetIndex: Comparable {
  fileprivate typealias Endpoint = (x: SourceIndex, y: TargetIndex)

  /// An encoding of change elements as an array of index pairs stored in
  /// `pathStorage[pathStartIndex...]`.
  ///
  /// This encoding allows the same storage to be used to run the difference
  /// algorithm, report the result, and repeat in place using
  /// `formChanges`.
  ///
  /// The collection of changes between XABCD and XYCD is:
  ///
  ///   [.match(0..<1, 0..<1), .remove(1..<3), .insert(1..<2),
  ///    .match(3..<5, 2..<4)]
  ///
  /// Which gets encoded as:
  ///
  ///   [(0, 0), (1, 1), (3, 1), (3, 2), (5, 4)]
  ///
  /// You can visualize it as a two-dimensional path composed of remove
  /// (horizontal), insert (vertical), and match (diagonal) segments:
  ///
  ///     X A B C D
  ///   X \ _ _
  ///   Y    |
  ///   C     \
  ///   D     \
  ///
  private var pathStorage: [Endpoint]

  /// The index in `pathStorage` of the first segment in the difference path.
  private var pathStartIndex: Int

  /// Creates a collection of changes from a difference path.
  fileprivate init(
    pathStorage: [Endpoint], pathStartIndex: Int
  ) {
    self.pathStorage = pathStorage
    self.pathStartIndex = pathStartIndex
  }

  /// Creates an empty collection of changes, i.e. the changes between two
  /// empty collections.
  private init() {
    self.pathStorage = []
    self.pathStartIndex = 0
  }
}

extension _CollectionChanges {
  /// A range of elements removed from the source, inserted in the target, or
  /// that the source and target have in common.
  fileprivate enum Element {
    case removed(Range<SourceIndex>)
    case inserted(Range<TargetIndex>)
    case matched(Range<SourceIndex>, Range<TargetIndex>)
  }
}

extension _CollectionChanges: RandomAccessCollection {
  fileprivate typealias Index = Int

  fileprivate var startIndex: Index {
    return 0
  }

  fileprivate var endIndex: Index {
    return Swift.max(0, pathStorage.endIndex - pathStartIndex - 1)
  }

  fileprivate func index(after i: Index) -> Index {
    return i + 1
  }

  fileprivate subscript(position: Index) -> Element {
    _internalInvariant((startIndex..<endIndex).contains(position))

    let current = pathStorage[position + pathStartIndex]
    let next = pathStorage[position + pathStartIndex + 1]

    if current.x != next.x && current.y != next.y {
      return .matched(current.x..<next.x, current.y..<next.y)
    } else if current.x != next.x {
      return .removed(current.x..<next.x)
    } else { // current.y != next.y
      return .inserted(current.y..<next.y)
    }
  }
}

extension _CollectionChanges: CustomStringConvertible {
  fileprivate var description: String {
    return _makeCollectionDescription()
  }
}

extension _CollectionChanges {
  /// Creates the collection of changes between `source` and `target`.
  ///
  /// - Runtime: O(*n* * *d*), where *n* is `source.count + target.count` and
  ///   *d* is the minimal number of inserted and removed elements.
  /// - Space: O(*d* * *d*), where *d* is the minimal number of inserted and
  ///   removed elements.
  fileprivate
  init<Source: BidirectionalCollection, Target: BidirectionalCollection>(
    from source: Source,
    to target: Target,
    by areEquivalent: (Source.Element, Target.Element) -> Bool
  ) where
    Source.Element == Target.Element,
    Source.Index == SourceIndex,
    Target.Index == TargetIndex
  {
    self.init()
    formChanges(from: source, to: target, by: areEquivalent)
  }

  /// Replaces `self` with the collection of changes between `source`
  /// and `target`.
  ///
  /// - Runtime: O(*n* * *d*), where *n* is `source.count + target.count` and
  ///   *d* is the minimal number of inserted and removed elements.
  /// - Space: O(*d*²), where *d* is the minimal number of inserted and
  ///   removed elements.
  private mutating func formChanges<
    Source: BidirectionalCollection,
    Target: BidirectionalCollection
  >(
    from source: Source,
    to target: Target,
    by areEquivalent: (Source.Element, Target.Element) -> Bool
  ) where
    Source.Element == Target.Element,
    Source.Index == SourceIndex,
    Target.Index == TargetIndex
  {
    let pathStart = (x: source.startIndex, y: target.startIndex)
    let pathEnd = (x: source.endIndex, y: target.endIndex)
    let matches = source._commonPrefix(with: target, by: areEquivalent)
    let (x, y) = (matches.0.endIndex, matches.1.endIndex)

    if pathStart == pathEnd {
      pathStorage.removeAll(keepingCapacity: true)
      pathStartIndex = 0
    } else if x == pathEnd.x || y == pathEnd.y {
      pathStorage.removeAll(keepingCapacity: true)
      pathStorage.append(pathStart)
      if pathStart != (x, y) && pathEnd != (x, y) {
        pathStorage.append((x, y))
      }
      pathStorage.append(pathEnd)
      pathStartIndex = 0
    } else {
      formChangesCore(from: source, to: target, x: x, y: y, by: areEquivalent)
    }
  }

  /// The core difference algorithm.
  ///
  /// - Precondition: There is at least one difference between `a` and `b`
  /// - Runtime: O(*n* * *d*), where *n* is `a.count + b.count` and
  ///   *d* is the number of inserts and removes.
  /// - Space: O(*d* * *d*), where *d* is the number of inserts and removes.
  private mutating func formChangesCore<
    Source: BidirectionalCollection,
    Target: BidirectionalCollection
  >(
    from a: Source,
    to b: Target,
    x: Source.Index,
    y: Target.Index,
    by areEquivalent: (Source.Element, Target.Element) -> Bool
  ) where
    Source.Element == Target.Element,
    Source.Index == SourceIndex,
    Target.Index == TargetIndex
  {
    // Written to correspond, as closely as possible, to the psuedocode in
    // Myers, E. "An O(ND) Difference Algorithm and Its Variations".
    //
    // See "FIGURE 2: The Greedy LCS/SES Algorithm" on p. 6 of the [paper].
    //
    // Note the following differences from the psuedocode in FIGURE 2:
    //
    // 1. FIGURE 2 relies on both *A* and *B* being Arrays. In a generic
    //  context, it isn't true that *y = x - k*, as *x*, *y*, *k* could
    //  all be different types, so we store both *x* and *y* in *V*.
    // 2. FIGURE 2 only reports the length of the LCS/SES. Reporting a
    //  solution path requires storing a copy of *V* (the search frontier)
    //  after each iteration of the outer loop.
    // 3. FIGURE 2 stops the search after *MAX* iterations. We run the loop
    //  until a solution is found. We also guard against incrementing past
    //  the end of *A* and *B*, both to satisfy the termination condition
    //  and because that would violate preconditions on collection.
    //
    // [paper]: http://www.xmailserver.org/diff2.pdf
    var (x, y) = (x, y)
    let (n, m) = (a.endIndex, b.endIndex)

    var v = _SearchState<Source.Index, Target.Index>(consuming: &pathStorage)

    v.appendFrontier(repeating: (x, y))
    var d = 1
    var delta = 0
    outer: while true {
      v.appendFrontier(repeating: (n, m))
      for k in stride(from: -d, through: d, by: 2) {
        if k == -d || (k != d && v[d - 1, k - 1].x < v[d - 1, k + 1].x) {
          (x, y) = v[d - 1, k + 1]
          if y != m { b.formIndex(after: &y) }
        } else {
          (x, y) = v[d - 1, k - 1]
          if x != n { a.formIndex(after: &x) }
        }

        let matches = a[x..<n]._commonPrefix(with: b[y..<m], by: areEquivalent)
        (x, y) = (matches.0.endIndex, matches.1.endIndex)
        v[d, k] = (x, y)

        if x == n && y == m {
          delta = k
          break outer
        }
      }
      d += 1
    }

    self = v.removeCollectionChanges(a: a, b: b, d: d, delta: delta)
  }
}

/// The search paths being explored.
fileprivate struct _SearchState<
  SourceIndex: Comparable,
  TargetIndex: Comparable
> {
  fileprivate typealias Endpoint = (x: SourceIndex, y: TargetIndex)

  /// The search frontier for each iteration.
  ///
  /// The nth iteration of the core algorithm requires storing n + 1 search
  /// path endpoints. Thus, the shape of the storage required is a triangle.
  private var endpoints = _LowerTriangularMatrix<Endpoint>()

  /// Creates an instance, taking the capacity of `storage` for itself.
  ///
  /// - Postcondition: `storage` is empty.
  fileprivate init(consuming storage: inout [Endpoint]) {
    storage.removeAll(keepingCapacity: true)
    swap(&storage, &endpoints.storage)
  }

  /// Returns the endpoint of the search frontier for iteration `d` on
  /// diagonal `k`.
  fileprivate subscript(d: Int, k: Int) -> Endpoint {
    get {
      _internalInvariant((-d...d).contains(k))
      _internalInvariant((d + k) % 2 == 0)
      return endpoints[d, (d + k) / 2]
    }
    set {
      _internalInvariant((-d...d).contains(k))
      _internalInvariant((d + k) % 2 == 0)
      endpoints[d, (d + k) / 2] = newValue
    }
  }

  /// Adds endpoints initialized to `repeatedValue` for the search frontier of
  /// the next iteration.
  fileprivate mutating func appendFrontier(repeating repeatedValue: Endpoint) {
    endpoints.appendRow(repeating: repeatedValue)
  }
}

extension _SearchState {
  /// Removes and returns `_CollectionChanges`, leaving `_SearchState` empty.
  ///
  /// - Precondition: There is at least one difference between `a` and `b`
  fileprivate mutating func removeCollectionChanges<
    Source: BidirectionalCollection,
    Target: BidirectionalCollection
  >(
    a: Source,
    b: Target,
    d: Int,
    delta: Int
  ) -> _CollectionChanges<Source.Index, Target.Index>
    where Source.Index == SourceIndex, Target.Index == TargetIndex
  {
    // Calculating the difference path is very similar to running the core
    // algorithm in reverse:
    //
    //   var k = delta
    //   for d in (1...d).reversed() {
    //     if k == -d || (k != d && self[d - 1, k - 1].x < self[d - 1, k + 1].x) {
    //       // insert of self[d - 1, k + 1].y
    //       k += 1
    //     } else {
    //       // remove of self[d - 1, k - 1].x
    //       k -= 1
    //     }
    //   }
    //
    // It is more complicated below because:
    //
    // 1. We want to include segments for matches
    // 2. We want to coallesce consecutive like segments
    // 3. We don't want to allocate, so we're overwriting the elements of
    //  endpoints.storage we've finished reading.

    let pathStart = (a.startIndex, b.startIndex)
    let pathEnd = (a.endIndex, b.endIndex)

    // `endpoints.storage` may need space for an additional element in order
    // to store the difference path when `d == 1`.
    //
    // `endpoints.storage` has `(d + 1) * (d + 2) / 2` elements stored,
    // but a difference path requires up to `2 + d * 2` elements[^1].
    //
    // If `d == 1`:
    //
    //   (1 + 1) * (1 + 2) / 2 < 2 + 1 * 2
    //             3 < 4
    //
    // `d == 1` is the only special case because:
    //
    // - It's a precondition that `d > 0`.
    // - Once `d >= 2` `endpoints.storage` will have sufficient space:
    //
    //     (d + 1) * (d + 2) / 2 = 2 + d * 2
    //    d * d - d - 2 = 0
    //     (d - 2) * (d + 1) = 0
    //    d = 2; d = -1
    //
    // [1]: An endpoint for every remove, insert, and match segment. (Recall
    // *d* is the minimal number of inserted and removed elements). If there
    // are no consecutive removes or inserts and every remove or insert is
    // sandwiched between matches, the path will need `2 + d * 2` elements.
    _internalInvariant(d > 0, "Must be at least one difference between `a` and `b`")
    if d == 1 {
      endpoints.storage.append(pathEnd)
    }

    var i = endpoints.storage.endIndex - 1
    // `isInsertion` tracks whether the element at `endpoints.storage[i]`
    // is an insertion (`true`), a removal (`false`), or a match (`nil`).
    var isInsertion: Bool? = nil
    var k = delta
    endpoints.storage[i] = pathEnd
    for d in (1...d).reversed() {
      if k == -d || (k != d && self[d - 1, k - 1].x < self[d - 1, k + 1].x) {
        let (x, y) = self[d - 1, k + 1]

        // There was match before this insert, so add a segment.
        if x != endpoints.storage[i].x {
          i -= 1; endpoints.storage[i] = (x, b.index(after: y))
          isInsertion = nil
        }

        // If the previous segment is also an insert, overwrite it.
        if isInsertion != .some(true) { i -= 1 }
        endpoints.storage[i] = (x, y)

        isInsertion = true
        k += 1
      } else {
        let (x, y) = self[d - 1, k - 1]

        // There was a match before this remove, so add a segment.
        if y != endpoints.storage[i].y {
          i -= 1; endpoints.storage[i] = (a.index(after: x), y)
          isInsertion = nil
        }

        // If the previous segment is also a remove, overwrite it.
        if isInsertion != .some(false) { i -= 1 }
        endpoints.storage[i] = (x, y)

        isInsertion = false
        k -= 1
      }
    }

    if pathStart != endpoints.storage[i] {
      i -= 1; endpoints.storage[i] = pathStart
    }

    let pathStorage = endpoints.storage
    endpoints.storage = []
    return _CollectionChanges(pathStorage: pathStorage, pathStartIndex: i)
  }
}

/// An index that counts its offset from the start of its collection.
private struct _CountingIndex<Base: Comparable>: Equatable {
  /// The position in the underlying collection.
  let base: Base
  /// The offset from the start index of the collection or `nil` if `self` is
  /// the end index.
  let offset: Int?
}

extension _CountingIndex: Comparable {
  fileprivate static func <(lhs: _CountingIndex, rhs: _CountingIndex) -> Bool {
    return (lhs.base, lhs.offset ?? Int.max) < (rhs.base, rhs.offset ?? Int.max)
  }
}

/// A collection that counts the offset of its indices from its start index.
///
/// You can use `_CountingIndexCollection` with algorithms on `Collection` to
/// calculate offsets of significance:
///
///   if let i = _CountingIndexCollection("Café").index(of: "f") {
///     print(i.offset)
///   }
///   // Prints "2"
///
/// - Note: The offset of `endIndex` is `nil`
private struct _CountingIndexCollection<Base: BidirectionalCollection> {
  private let base: Base

  fileprivate init(_ base: Base) {
    self.base = base
  }
}

extension _CountingIndexCollection : BidirectionalCollection {
  fileprivate typealias Index = _CountingIndex<Base.Index>
  fileprivate typealias Element = Base.Element

  fileprivate var startIndex: Index {
    return Index(base: base.startIndex, offset: base.isEmpty ? nil : 0)
  }

  fileprivate var endIndex: Index {
    return Index(base: base.endIndex, offset: nil)
  }

  fileprivate func index(after i: Index) -> Index {
    let next = base.index(after: i.base)
    return Index(
      base: next, offset: next == base.endIndex ? nil : i.offset! + 1)
  }

  fileprivate func index(before i: Index) -> Index {
    let prev = base.index(before: i.base)
    return Index(
      base: prev, offset: prev == base.endIndex ? nil : i.offset! + 1)
  }

  fileprivate subscript(position: Index) -> Element {
    return base[position.base]
  }
}

/// Returns the nth [triangular number].
///
/// [triangular number]: https://en.wikipedia.org/wiki/Triangular_number
fileprivate func _triangularNumber(_ n: Int) -> Int {
  return n * (n + 1) / 2
}

/// A square matrix that only provides subscript access to elements on, or
/// below, the main diagonal.
///
/// A [lower triangular matrix] can be dynamically grown:
///
///   var m = _LowerTriangularMatrix<Int>()
///   m.appendRow(repeating: 1)
///   m.appendRow(repeating: 2)
///   m.appendRow(repeating: 3)
///
///   assert(Array(m.rowMajorOrder) == [
///     1,
///     2, 2,
///     3, 3, 3,
///   ])
///
/// [lower triangular matrix]: http://en.wikipedia.org/wiki/Triangular_matrix
fileprivate struct _LowerTriangularMatrix<Element> {
  /// The matrix elements stored in [row major order][rmo].
  ///
  /// [rmo]: http://en.wikipedia.org/wiki/Row-_and_column-major_order
  fileprivate var storage: [Element] = []

  /// The dimension of the matrix.
  ///
  /// Being a square matrix, the number of rows and columns are equal.
  fileprivate var dimension: Int = 0

  fileprivate subscript(row: Int, column: Int) -> Element {
    get {
      _internalInvariant((0...row).contains(column))
      return storage[_triangularNumber(row) + column]
    }
    set {
      _internalInvariant((0...row).contains(column))
      storage[_triangularNumber(row) + column] = newValue
    }
  }

  fileprivate mutating func appendRow(repeating repeatedValue: Element) {
    dimension += 1
    storage.append(contentsOf: repeatElement(repeatedValue, count: dimension))
  }
}

extension _LowerTriangularMatrix {
  /// A collection that visits the elements in the matrix in [row major
  /// order][rmo].
  ///
  /// [rmo]: http://en.wikipedia.org/wiki/Row-_and_column-major_order
  fileprivate struct RowMajorOrder : RandomAccessCollection {
    private var base: _LowerTriangularMatrix

    fileprivate init(base: _LowerTriangularMatrix) {
      self.base = base
    }

    fileprivate var startIndex: Int {
      return base.storage.startIndex
    }

    fileprivate var endIndex: Int {
      return base.storage.endIndex
    }

    fileprivate func index(after i: Int) -> Int {
      return i + 1
    }

    fileprivate func index(before i: Int) -> Int {
      return i - 1
    }

    fileprivate subscript(position: Int) -> Element {
      return base.storage[position]
    }
  }

  fileprivate var rowMajorOrder: RowMajorOrder {
    return RowMajorOrder(base: self)
  }

  fileprivate subscript(row r: Int) -> Slice<RowMajorOrder> {
    return rowMajorOrder[_triangularNumber(r)..<_triangularNumber(r + 1)]
  }
}

extension _LowerTriangularMatrix: CustomStringConvertible {
  fileprivate var description: String {
    var rows: [[Element]] = []
    for row in 0..<dimension {
      rows.append(Array(self[row: row]))
    }
    return String(describing: rows)
  }
}
