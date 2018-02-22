//===--- FlatMap.swift ----------------------------------------------------===//
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

// MARK - FlatMap section

extension LazySequenceProtocol {
  /// Returns the concatenated results of mapping the given transformation over
  /// this sequence.
  ///
  /// Use this method to receive a single-level sequence when your
  /// transformation produces a sequence or collection for each element.
  /// Calling `flatMap(_:)` on a sequence `s` is equivalent to calling
  /// `s.map(transform).joined()`.
  ///
  /// - Complexity: O(1)
  @_inlineable // FIXME(sil-serialize-all)
  public func flatMap<SegmentOfResult>(
    _ transform: @escaping (Elements.Element) -> SegmentOfResult
  ) -> LazySequence<
    FlattenSequence<LazyMapSequence<Elements, SegmentOfResult>>> {
    return self.map(transform).joined()
  }
}

extension LazyCollectionProtocol {
  /// Returns the concatenated results of mapping the given transformation over
  /// this collection.
  ///
  /// Use this method to receive a single-level collection when your
  /// transformation produces a collection for each element.
  /// Calling `flatMap(_:)` on a collection `c` is equivalent to calling
  /// `c.map(transform).joined()`.
  ///
  /// - Complexity: O(1)
  @_inlineable // FIXME(sil-serialize-all)
  public func flatMap<SegmentOfResult>(
    _ transform: @escaping (Elements.Element) -> SegmentOfResult
  ) -> LazyCollection<
    FlattenCollection<
      LazyMapCollection<Elements, SegmentOfResult>>
  > {
    return self.map(transform).joined()
  }
}

// MARK - CompactMap Section

/// A `Sequence` whose elements consist of those in a `Base`
/// `Sequence` passed through a transform function returning `Element?`
/// that resulted in a non-nil output.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
@_fixed_layout // FIXME(sil-serialize-all)
public struct LazyCompactMapSequence<Base : Sequence, Element> {
  @_versioned // FIXME(sil-serialize-all)
  internal var _base: Base
  @_versioned // FIXME(sil-serialize-all)
  internal let _transform: (Base.Element) -> Element?

  /// Creates an instance with elements `transform(x)!` for each element
  /// `x` of base where `transform(x)` is not nil.
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal init(_base: Base, transform: @escaping (Base.Element) -> Element?) {
    self._base = _base
    self._transform = transform
  } 
}

extension LazyCompactMapSequence {
  /// An iterator over the transformed elements traversed by some base
  /// iterator where the transform doesn't return `nil`
  ///
  /// - Note: This is the associated `Iterator` of `LazyFilterSequence`
  /// and `LazyFilterCollection`.
  @_fixed_layout
  public struct Iterator {
    /// The underlying iterator whose elements are being compactMapped
    @_inlineable
    public var base: Base.Iterator { return _base }

    @_versioned // FIXME(sil-serialize-all)
    internal var _base: Base.Iterator
    @_versioned // FIXME(sil-serialize-all)
    internal let _transform: (Base.Element) -> Element?

    /// Creates an instance that produces the elements `x` of `base`
    /// after being transformed by `transform` where `transform`
    /// returns a non-nil value
    @_inlineable // FIXME(sil-serialize-all)
    @_versioned // FIXME(sil-serialize-all)
    internal init(
      _base: Base.Iterator,
      _transform: @escaping (Base.Element) -> Element?
    ) {
      self._base = _base
      self._transform = _transform
    }
  }
}

extension LazyCompactMapSequence.Iterator: IteratorProtocol, Sequence {
  /// Advances to the next element and returns it, or `nil` if no next element
  /// exists.
  ///
  /// Once `nil` has been returned, all subsequent calls return `nil`.
  ///
  /// - Precondition: `next()` has not been applied to a copy of `self`
  ///   since the copy was made.
  @_inlineable // FIXME(sil-serialize-all)
  public mutating func next() -> Element? {
    while let n = _base.next() {
      if let transformed = _transform(n) {
        return transformed
      }
    }
    return nil
  }
}

extension LazyCompactMapSequence: LazySequenceProtocol {
  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @_inlineable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator(), _transform: _transform)
  }
}

/// A `Collection` whose elements consist of those in a `Base` 
/// `Collection` passed through a transform function returning `Element?`
/// where that transform produces a non-nil value.
/// These elements are computed lazily, each time they're read, by
/// calling the transform function on a base element.
///
/// - Note: The performance of accessing `startIndex`, `first`, any methods
///   that depend on `startIndex`, or of advancing an index depends
///   on how sparsely the transform function is nonnil, and may not offer
///   the usual performance given by `Collection`. Be aware, therefore, that
///   general operations on `LazyCompactMapCollection` instances may not
///   have the documented complexity.
@_fixed_layout
public struct LazyCompactMapCollection<Base: Collection, Element> {
  @_versioned // FIXME(sil-serialize-all)
  internal var _base: Base
  @_versioned // FIXME(sil-serialize-all)
  internal var _transform: (Base.Element) -> Element?

  /// Creates an instance with elements `transform(x)!` for each element
  /// `x` of base where `transform(x)` is not nil.
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal init(_base: Base, transform: @escaping (Base.Element) -> Element?) {
    self._base = _base
    self._transform = transform
  }
}

extension LazyCompactMapCollection: LazySequenceProtocol {
  public typealias Iterator = LazyCompactMapSequence<Base, Element>.Iterator
  public typealias SubSequence = LazyCompactMapCollection<Base.SubSequence, Element>

  // Any estimate of the number of elements that pass `_transform` requires
  // iterating the collection and evaluating each element, which can be costly,
  // is unexpected, and usually doesn't pay for itself in saving time through
  // preventing intermediate reallocations. (SR-4164)
  @_inlineable // FIXME(sil-serialize-all)
  public var underestimatedCount: Int { return 0 }

  @_inlineable // FIXME(sil-serialize-all)
  public func _copyToContiguousArray() -> ContiguousArray<Element> {
    // The default implementation of `_copyToContiguousArray` queries the
    // `count` property, which evaluates `_transform` for every element --
    // see the note above `underestimatedCount`. Here we treat `self` as a
    // sequence and only rely on underestimated count.
    return _copySequenceToContiguousArray(self)
  }

  /// Returns an iterator over the elements of this sequence.
  ///
  /// - Complexity: O(1).
  @_inlineable // FIXME(sil-serialize-all)
  public func makeIterator() -> Iterator {
    return Iterator(_base: _base.makeIterator(), _transform: _transform)
  }
}

extension LazyCompactMapCollection: LazyCollectionProtocol {
  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  public typealias Index = Base.Index

  /// The position of the first element in a non-empty collection.
  ///
  /// In an empty collection, `startIndex == endIndex`.
  ///
  /// - Complexity: O(*n*), where *n* is the ratio between unfiltered and
  ///   filtered collection counts.
  @_inlineable // FIXME(sil-serialize-all)
  public var startIndex: Index {
    var index = _base.startIndex
    while index != _base.endIndex && _transform(_base[index]) == nil {
      _base.formIndex(after: &index)
    }
    return index
  }

  /// The collection's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// `endIndex` is always reachable from `startIndex` by zero or more
  /// applications of `index(after:)`.
  @_inlineable // FIXME(sil-serialize-all)
  public var endIndex: Index {
    return _base.endIndex
  }

  // TODO: swift-3-indexing-model - add docs
  @_inlineable // FIXME(sil-serialize-all)
  public func index(after i: Index) -> Index {
    var i = i
    formIndex(after: &i)
    return i
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func formIndex(after i: inout Index) {
    // TODO: swift-3-indexing-model: _failEarlyRangeCheck i?
    _precondition(i != _base.endIndex, "Can't advance past endIndex")
    repeat {
      _base.formIndex(after: &i)
    } while i != _base.endIndex && _transform(_base[i]) == nil
  }

  @inline(__always)
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _advanceIndex(_ i: inout Index, step: Int) {
    repeat {
      _base.formIndex(&i, offsetBy: step)
    } while i != _base.endIndex && _transform(_base[i]) == nil
  }

  @inline(__always)
  @_inlineable // FIXME(sil-serialize-all)
  @_versioned // FIXME(sil-serialize-all)
  internal func _ensureBidirectional(step: Int) {
    // FIXME: This seems to be the best way of checking whether _base is
    // forward only without adding an extra protocol requirement.
    // index(_:offsetBy:limitedBy:) is chosen becuase it is supposed to return
    // nil when the resulting index lands outside the collection boundaries,
    // and therefore likely does not trap in these cases.
    if step < 0 {
      _ = _base.index(
        _base.endIndex, offsetBy: step, limitedBy: _base.startIndex
      )
    }
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func distance(from start: Index, to end: Index) -> Int {
    // The following line makes sure that distance(from:to:) is invoked on the
    // _base at least once, to trigger a _precondition in forward only
    // collections.
    _ = _base.distance(from: start, to: end)
    var _start: Index
    let _end: Index
    let step: Int
    if start > end {
      _start = end
      _end = start
      step = -1
    }
    else {
      _start = start
      _end = end
      step = 1
    }
    var count = 0
    while _start != _end {
      count += step
      formIndex(after: &_start)
    }
    return count
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func index(_ i: Index, offsetBy n: Int) -> Index {
    var i = i
    formIndex(&i, offsetBy: n)
    return i
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func formIndex(_ i: inout Index, offsetBy n: Int) {
    let step = n.signum()
    // The following line makes sure that index(_:offsetBy:) is invoked on the
    // _base at least once, to trigger a _precondition in forward only
    // collections.
    _ensureBidirectional(step: step)
    for _ in 0 ..< abs(numericCast(n)) {
      _advanceIndex(&i, step: step)
    }
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func index(
    _ i: Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Index? {
    var i = i
    if formIndex(&i, offsetBy: n, limitedBy: limit) {
      return i
    }
    return nil
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func formIndex(
    _ i: inout Index, offsetBy n: Int, limitedBy limit: Index
  ) -> Bool {
    let step = n.signum()
    // The following line makes sure that index(_:offsetBy:limitedBy:) is
    // invoked on the _base at least once, to trigger a _precondition in
    // forward only collections.
    _ensureBidirectional(step: step)
    for _ in 0 ..< abs(numericCast(n)) {
      if i == limit {
        return false
      }
      _advanceIndex(&i, step: step)
    }
    return true
  }

  /// Accesses the transformed element at `position`
  ///
  /// - Precondition: `position` is a valid position in `self` and
  /// `position != endIndex`.
  @_inlineable // FIXME(sil-serialize-all)
  public subscript(position: Index) -> Element {
    return _transform(_base[position])!
  }

  @_inlineable // FIXME(sil-serialize-all)
  public subscript(bounds: Range<Index>) -> SubSequence {
    return SubSequence(_base: _base[bounds], transform: _transform)
  }
}

extension LazyCompactMapCollection: BidirectionalCollection
    where Base: BidirectionalCollection {
  
  @_inlineable // FIXME(sil-serialize-all)
  public func index(before i: Index) -> Index {
    var i = i
    formIndex(before: &i)
    return i
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func formIndex(before i: inout Index) {
    precondition(i != _base.startIndex, "Can't retreat before startIndex")
    repeat {
      _base.formIndex(before: &i)
    } while _transform(_base[i]) == nil
  }
}

extension LazySequenceProtocol {
  /// Returns the non-`nil` results of mapping the given transformation over
  /// this sequence.
  ///
  /// Use this method to receive a sequence of nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this sequence
  ///   as its argument and returns an optional value.
  ///
  /// - Complexity: O(1)
  @_inlineable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyCompactMapSequence<Elements, ElementOfResult> {
    return LazyCompactMapSequence(_base: self.elements, transform: transform)
  }

  /// Returns the non-`nil` results of mapping the given transformation over
  /// this sequence.
  ///
  /// Use this method to receive a sequence of nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this sequence
  ///   as its argument and returns an optional value.
  ///
  /// - Complexity: O(1)
  @inline(__always)
  @available(swift, deprecated: 4.1, renamed: "compactMap(_:)",
    message: "Please use compactMap(_:) for the case where closure returns an optional value")
  public func flatMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyMapSequence<
    LazyFilterSequence<
      LazyMapSequence<Elements, ElementOfResult?>>,
    ElementOfResult
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
  }
}

extension LazyCollectionProtocol {
  /// Returns the non-`nil` results of mapping the given transformation over
  /// this collection.
  ///
  /// Use this method to receive a collection of nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  ///   collection as its argument and returns an optional value.
  ///
  /// - Complexity: O(1)
  @_inlineable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyCompactMapCollection<Elements, ElementOfResult> {
    return LazyCompactMapCollection(_base: self.elements, transform: transform)
  }

  /// Returns the non-`nil` results of mapping the given transformation over
  /// this collection.
  ///
  /// Use this method to receive a collection of nonoptional values when your
  /// transformation produces an optional value.
  ///
  /// - Parameter transform: A closure that accepts an element of this
  ///   collection as its argument and returns an optional value.
  ///
  /// - Complexity: O(1)
  @available(swift, deprecated: 4.1, renamed: "compactMap(_:)",
    message: "Please use compactMap(_:) for the case where closure returns an optional value")
  @_inlineable // FIXME(sil-serialize-all)
  public func flatMap<ElementOfResult>(
    _ transform: @escaping (Elements.Element) -> ElementOfResult?
  ) -> LazyMapCollection<
    LazyFilterCollection<
      LazyMapCollection<Elements, ElementOfResult?>>,
    ElementOfResult
  > {
    return self.map(transform).filter { $0 != nil }.map { $0! }
  }
}

extension LazyMapSequence {
  @_inlineable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Element) -> ElementOfResult?
  ) -> LazyCompactMapSequence<Base, ElementOfResult> {
    let mytransform = self._transform
    return LazyCompactMapSequence<Base, ElementOfResult>(
      _base: self._base,
      transform: { transform(mytransform($0)) }
    )
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func filter(
    _ isIncluded: @escaping (Element) -> Bool
  ) -> LazyCompactMapSequence<Base, Element> {
    let mytransform = self._transform
    return LazyCompactMapSequence<Base, Element>(
      _base: self._base,
      transform: {
        let transformed = mytransform($0)
        return isIncluded(transformed) ? transformed : nil
      }
    )
  }
}

extension LazyMapCollection {
  @_inlineable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Element) -> ElementOfResult?
  ) -> LazyCompactMapCollection<Base, ElementOfResult> {
    let mytransform = self._transform
    return LazyCompactMapCollection<Base, ElementOfResult>(
      _base: self._base,
      transform: { transform(mytransform($0)) }
    )
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func filter(
    _ isIncluded: @escaping (Element) -> Bool
  ) -> LazyCompactMapCollection<Base, Element> {
    let mytransform = self._transform
    return LazyCompactMapCollection<Base, Element>(
      _base: self._base,
      transform: {
        let transformed = mytransform($0)
        return isIncluded(transformed) ? transformed : nil
      }
    )
  }
}

extension LazyFilterSequence {
  @_inlineable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Base.Element) -> ElementOfResult?
  ) -> LazyCompactMapSequence<Base, ElementOfResult> {
    let mypredicate = self._predicate
    return LazyCompactMapSequence<Base, ElementOfResult>(
      _base: self._base,
      transform: { mypredicate($0) ? transform($0) : nil }
    )
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func map<ElementOfResult>(
    _ transform: @escaping (Base.Element) -> ElementOfResult
  ) -> LazyCompactMapSequence<Base, ElementOfResult> {
    let mypredicate = self._predicate
    return LazyCompactMapSequence<Base, ElementOfResult>(
      _base: self._base,
      transform: { mypredicate($0) ? transform($0) : nil }
    )
  }
}

extension LazyFilterCollection {
  @_inlineable // FIXME(sil-serialize-all)
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Base.Element) -> ElementOfResult?
  ) -> LazyCompactMapCollection<Base, ElementOfResult> {
    let mypredicate = self._predicate
    return LazyCompactMapCollection<Base, ElementOfResult>(
      _base: self._base,
      transform: { mypredicate($0) ? transform($0) : nil }
    )
  }

  @_inlineable // FIXME(sil-serialize-all)
  public func map<ElementOfResult>(
    _ transform: @escaping (Base.Element) -> ElementOfResult
  ) -> LazyCompactMapCollection<Base, ElementOfResult> {
    let mypredicate = self._predicate
    return LazyCompactMapCollection<Base, ElementOfResult>(
      _base: self._base,
      transform: { mypredicate($0) ? transform($0) : nil }
    )
  }
}

extension LazyCompactMapSequence {
  @_inlineable
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Element) -> ElementOfResult?
  ) -> LazyCompactMapSequence<Base, ElementOfResult> {
    let mytransform = self._transform
    return LazyCompactMapSequence<Base, ElementOfResult>(
      _base: self._base,
      transform: {
        guard let halfTransformed = mytransform($0) else { return nil }
        return transform(halfTransformed)
      }
    )
  }

  @_inlineable
  public func map<ElementOfResult>(
    _ transform: @escaping (Element) -> ElementOfResult
  ) -> LazyCompactMapSequence<Base, ElementOfResult> {
    let mytransform = self._transform
    return LazyCompactMapSequence<Base, ElementOfResult>(
      _base: self._base,
      transform: {
        guard let halfTransformed = mytransform($0) else { return nil }
        return transform(halfTransformed)
      }
    )
  }

  @_inlineable
  public func filter(
    _ isIncluded: @escaping (Element) -> Bool
  ) -> LazyCompactMapSequence<Base, Element> {
    let mytransform = self._transform
    return LazyCompactMapSequence<Base, Element>(
      _base: self._base,
      transform: {
        guard let halfTransformed = mytransform($0), isIncluded(halfTransformed) else { return nil }
        return halfTransformed
      }
    )
  }
}

extension LazyCompactMapCollection {
  @_inlineable
  public func compactMap<ElementOfResult>(
    _ transform: @escaping (Element) -> ElementOfResult?
  ) -> LazyCompactMapCollection<Base, ElementOfResult> {
    let mytransform = self._transform
    return LazyCompactMapCollection<Base, ElementOfResult>(
      _base: self._base,
      transform: {
        guard let halfTransformed = mytransform($0) else { return nil }
        return transform(halfTransformed)
      }
    )
  }

  @_inlineable
  public func map<ElementOfResult>(
    _ transform: @escaping (Element) -> ElementOfResult
  ) -> LazyCompactMapCollection<Base, ElementOfResult> {
    let mytransform = self._transform
    return LazyCompactMapCollection<Base, ElementOfResult>(
      _base: self._base,
      transform: {
        guard let halfTransformed = mytransform($0) else { return nil }
        return transform(halfTransformed)
      }
    )
  }

  @_inlineable
  public func filter(
    _ isIncluded: @escaping (Element) -> Bool
  ) -> LazyCompactMapCollection<Base, Element> {
    let mytransform = self._transform
    return LazyCompactMapCollection<Base, Element>(
      _base: self._base,
      transform: {
        guard let halfTransformed = mytransform($0), isIncluded(halfTransformed) else { return nil }
        return halfTransformed
      }
    )
  }
}