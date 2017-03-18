//===----------------------------------------------------------------------===//
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

/// A type that provides subscript access to its elements.
///
/// In most cases, it's best to ignore this protocol and use the
/// `MutableCollection` protocol instead, because it has a more complete
/// interface.
@available(*, deprecated, message: "it will be removed in Swift 4.0.  Please use 'MutableCollection' instead")
public typealias MutableIndexable = _MutableIndexable
public protocol _MutableIndexable : _Indexable {
  // FIXME(ABI)#52 (Recursive Protocol Constraints): there is no reason for this protocol
  // to exist apart from missing compiler features that we emulate with it.
  // rdar://problem/20531108
  //
  // This protocol is almost an implementation detail of the standard
  // library; it is used to deduce things like the `SubSequence` and
  // `Iterator` type from a minimal collection, but it is also used in
  // exposed places like as a constraint on `IndexingIterator`.

  /// A type that represents a valid position in the collection.
  ///
  /// Valid indices consist of the position of every element and a
  /// "past the end" position that's not valid for use as a subscript.
  // TODO: swift-3-indexing-model - Index only needs to be comparable or must be comparable..?
  associatedtype Index

  /// The position of the first element in a nonempty collection.
  ///
  /// If the collection is empty, `startIndex` is equal to `endIndex`.
  var startIndex: Index { get }

  /// The collection's "past the end" position---that is, the position one
  /// greater than the last valid subscript argument.
  ///
  /// When you need a range that includes the last element of a collection, use
  /// the half-open range operator (`..<`) with `endIndex`. The `..<` operator
  /// creates a range that doesn't include the upper bound, so it's always
  /// safe to use with `endIndex`. For example:
  ///
  ///     let numbers = [10, 20, 30, 40, 50]
  ///     if let index = numbers.index(of: 30) {
  ///         print(numbers[index ..< numbers.endIndex])
  ///     }
  ///     // Prints "[30, 40, 50]"
  ///
  /// If the collection is empty, `endIndex` is equal to `startIndex`.
  var endIndex: Index { get }

  // The declaration of _Element and subscript here is a trick used to
  // break a cyclic conformance/deduction that Swift can't handle.  We
  // need something other than a Collection.Iterator.Element that can
  // be used as IndexingIterator<T>'s Element.  Here we arrange for
  // the Collection itself to have an Element type that's deducible from
  // its subscript.  Ideally we'd like to constrain this Element to be the same
  // as Collection.Iterator.Element (see below), but we have no way of
  // expressing it today.
  associatedtype _Element

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
  subscript(position: Index) -> _Element { get set }

  /// A collection that represents a contiguous subrange of the collection's
  /// elements.
  associatedtype SubSequence
  
  /// Accesses a contiguous subrange of the collection's elements.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  /// This example demonstrates getting a slice of an array of strings, finding
  /// the index of one of the strings in the slice, and then using that index
  /// in the original array.
  ///
  ///     let streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.index(of: "Evarts")    // 4
  ///     streets[index!] = "Eustace"
  ///     print(streets[index!])
  ///     // Prints "Eustace"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  subscript(bounds: Range<Index>) -> SubSequence { get set }
  
  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(bounds.contains(index))
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  func _failEarlyRangeCheck(_ index: Index, bounds: Range<Index>)

  /// Performs a range check in O(1), or a no-op when a range check is not
  /// implementable in O(1).
  ///
  /// The range check, if performed, is equivalent to:
  ///
  ///     precondition(
  ///       bounds.contains(range.lowerBound) ||
  ///       range.lowerBound == bounds.upperBound)
  ///     precondition(
  ///       bounds.contains(range.upperBound) ||
  ///       range.upperBound == bounds.upperBound)
  ///
  /// Use this function to perform a cheap range check for QoI purposes when
  /// memory safety is not a concern.  Do not rely on this range check for
  /// memory safety.
  ///
  /// The default implementation for forward and bidirectional indices is a
  /// no-op.  The default implementation for random access indices performs a
  /// range check.
  ///
  /// - Complexity: O(1).
  func _failEarlyRangeCheck(_ range: Range<Index>, bounds: Range<Index>)

  /// Returns the position immediately after the given index.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  /// - Returns: The index value immediately after `i`.
  func index(after i: Index) -> Index

  /// Replaces the given index with its successor.
  ///
  /// - Parameter i: A valid index of the collection. `i` must be less than
  ///   `endIndex`.
  func formIndex(after i: inout Index)
}

// TODO: swift-3-indexing-model - review the following
/// A collection that supports subscript assignment.
///
/// Collections that conform to `MutableCollection` gain the ability to
/// change the value of their elements. This example shows how you can
/// modify one of the names in an array of students.
///
///     var students = ["Ben", "Ivy", "Jordell", "Maxime"]
///     if let i = students.index(of: "Maxime") {
///         students[i] = "Max"
///     }
///     print(students)
///     // Prints "["Ben", "Ivy", "Jordell", "Max"]"
///
/// In addition to changing the value of an individual element, you can also
/// change the values of a slice of elements in a mutable collection. For
/// example, you can sort *part* of a mutable collection by calling the
/// mutable `sort()` method on a subscripted subsequence. Here's an
/// example that sorts the first half of an array of integers:
///
///     var numbers = [15, 40, 10, 30, 60, 25, 5, 100]
///     numbers[0..<4].sort()
///     print(numbers)
///     // Prints "[10, 15, 30, 40, 60, 25, 5, 100]"
///
/// The `MutableCollection` protocol allows changing the values of a
/// collection's elements but not the length of the collection itself. For
/// operations that require adding or removing elements, see the
/// `RangeReplaceableCollection` protocol instead.
///
/// Conforming to the MutableCollection Protocol
/// ============================================
///
/// To add conformance to the `MutableCollection` protocol to your own
/// custom collection, upgrade your type's subscript to support both read
/// and write access.
/// 
/// A value stored into a subscript of a `MutableCollection` instance must
/// subsequently be accessible at that same position. That is, for a mutable
/// collection instance `a`, index `i`, and value `x`, the two sets of
/// assignments in the following code sample must be equivalent:
///
///     a[i] = x
///     let y = a[i]
///     
///     // Must be equivalent to:
///     a[i] = x
///     let y = x
public protocol MutableCollection : _MutableIndexable, Collection {
  // FIXME(ABI)#181: should be constrained to MutableCollection
  // (<rdar://problem/20715009> Implement recursive protocol
  // constraints)
  /// A collection that represents a contiguous subrange of the collection's
  /// elements.
  associatedtype SubSequence : Collection /*: MutableCollection*/
    = MutableSlice<Self>

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
  subscript(position: Index) -> Iterator.Element {get set}

  /// Accesses a contiguous subrange of the collection's elements.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  /// This example demonstrates getting a slice of an array of strings, finding
  /// the index of one of the strings in the slice, and then using that index
  /// in the original array.
  ///
  ///     let streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.index(of: "Evarts")    // 4
  ///     streets[index!] = "Eustace"
  ///     print(streets[index!])
  ///     // Prints "Eustace"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  subscript(bounds: Range<Index>) -> SubSequence {get set}

  /// Reorders the elements of the collection such that all the elements
  /// that match the given predicate are after all the elements that do
  /// not match the predicate.
  ///
  /// After partitioning a collection, there is a pivot index `p` where
  /// no element before `p` satisfies the `belongsInSecondPartition`
  /// predicate and every element at or after `p` satisfies
  /// `belongsInSecondPartition`.
  ///
  /// In the following example, an array of numbers is partitioned by a
  /// predicate that matches elements greater than 30.
  ///
  ///     var numbers = [30, 40, 20, 30, 30, 60, 10]
  ///     let p = numbers.partition(by: { $0 > 30 })
  ///     // p == 5
  ///     // numbers == [30, 10, 20, 30, 30, 60, 40]
  ///
  /// The `numbers` array is now arranged in two partitions. The first
  /// partition, `numbers.prefix(upTo: p)`, is made up of the elements that
  /// are not greater than 30. The second partition, `numbers.suffix(from: p)`,
  /// is made up of the elements that *are* greater than 30.
  ///
  ///     let first = numbers.prefix(upTo: p)
  ///     // first == [30, 10, 20, 30, 30]
  ///     let second = numbers.suffix(from: p)
  ///     // second == [60, 40]
  ///
  /// - Parameter belongsInSecondPartition: A predicate used to partition
  ///   the collection. All elements satisfying this predicate are ordered
  ///   after all elements not satisfying it.
  /// - Returns: The index of the first element in the reordered collection
  ///   that matches `belongsInSecondPartition`. If no elements in the
  ///   collection match `belongsInSecondPartition`, the returned index is
  ///   equal to the collection's `endIndex`.
  ///
  /// - Complexity: O(*n*)
  mutating func partition(
    by belongsInSecondPartition: (Iterator.Element) throws -> Bool
  ) rethrows -> Index
  
  /// Call `body(p)`, where `p` is a pointer to the collection's
  /// mutable contiguous storage.  If no such storage exists, it is
  /// first created.  If the collection does not support an internal
  /// representation in a form of mutable contiguous storage, `body` is not
  /// called and `nil` is returned.
  ///
  /// Often, the optimizer can eliminate bounds- and uniqueness-checks
  /// within an algorithm, but when that fails, invoking the
  /// same algorithm on `body`\ 's argument lets you trade safety for
  /// speed.
  mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (UnsafeMutablePointer<Iterator.Element>, Int) throws -> R
  ) rethrows -> R?
  // FIXME(ABI)#53 (Type Checker): the signature should use
  // UnsafeMutableBufferPointer, but the compiler can't handle that.
  //
  // <rdar://problem/21933004> Restore the signature of
  // _withUnsafeMutableBufferPointerIfSupported() that mentions
  // UnsafeMutableBufferPointer
}

// TODO: swift-3-indexing-model - review the following
extension MutableCollection {
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (UnsafeMutablePointer<Iterator.Element>, Int) throws -> R
  ) rethrows -> R? {
    return nil
  }

  /// Accesses a contiguous subrange of the collection's elements.
  ///
  /// The accessed slice uses the same indices for the same elements as the
  /// original collection. Always use the slice's `startIndex` property
  /// instead of assuming that its indices start at a particular value.
  ///
  /// This example demonstrates getting a slice of an array of strings, finding
  /// the index of one of the strings in the slice, and then using that index
  /// in the original array.
  ///
  ///     let streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.index(of: "Evarts")    // 4
  ///     streets[index!] = "Eustace"
  ///     print(streets[index!])
  ///     // Prints "Eustace"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  public subscript(bounds: Range<Index>) -> MutableSlice<Self> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return MutableSlice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }
}

extension MutableCollection where Self: BidirectionalCollection {
  public subscript(bounds: Range<Index>) -> MutableBidirectionalSlice<Self> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return MutableBidirectionalSlice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }
}

extension MutableCollection where Self: RandomAccessCollection {
  public subscript(bounds: Range<Index>) -> MutableRandomAccessSlice<Self> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return MutableRandomAccessSlice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }
}

@available(*, unavailable, renamed: "MutableCollection")
public typealias MutableCollectionType = MutableCollection

@available(*, unavailable, message: "Please use 'Collection where SubSequence : MutableCollection'")
public typealias MutableSliceable = Collection
