//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// A collection that supports subscript assignment.
///
/// Collections that conform to `MutableCollection` gain the ability to
/// change the value of their elements. This example shows how you can
/// modify one of the names in an array of students.
///
///     var students = ["Ben", "Ivy", "Jordell", "Maxime"]
///     if let i = students.firstIndex(of: "Maxime") {
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
public protocol MutableCollection: Collection
where SubSequence: MutableCollection
{
  // FIXME: Associated type inference requires these.
  override associatedtype Element
  override associatedtype Index
  override associatedtype SubSequence

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
  @_borrowed
  override subscript(position: Index) -> Element { get set }

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
  ///     var streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.firstIndex(of: "Evarts")    // 4
  ///     streets[index!] = "Eustace"
  ///     print(streets[index!])
  ///     // Prints "Eustace"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  ///
  /// - Complexity: O(1)
  override subscript(bounds: Range<Index>) -> SubSequence { get set }

  /// Reorders the elements of the collection such that all the elements
  /// that match the given predicate are after all the elements that don't
  /// match.
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
  /// partition, `numbers[..<p]`, is made up of the elements that
  /// are not greater than 30. The second partition, `numbers[p...]`,
  /// is made up of the elements that *are* greater than 30.
  ///
  ///     let first = numbers[..<p]
  ///     // first == [30, 10, 20, 30, 30]
  ///     let second = numbers[p...]
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
  /// - Complexity: O(*n*), where *n* is the length of the collection.
  mutating func partition(
    by belongsInSecondPartition: (Element) throws -> Bool
  ) rethrows -> Index

  /// Exchanges the values at the specified indices of the collection.
  ///
  /// Both parameters must be valid indices of the collection and not
  /// equal to `endIndex`. Passing the same index as both `i` and `j` has no
  /// effect.
  ///
  /// - Parameters:
  ///   - i: The index of the first value to swap.
  ///   - j: The index of the second value to swap.
  ///
  /// - Complexity: O(1)
  mutating func swapAt(_ i: Index, _ j: Index)
  
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
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R?

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
  mutating func withContiguousMutableStorageIfAvailable<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R?
}

// TODO: swift-3-indexing-model - review the following
extension MutableCollection {
  @inlinable
  public mutating func _withUnsafeMutableBufferPointerIfSupported<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
  ) rethrows -> R? {
    return nil
  }

  @inlinable
  public mutating func withContiguousMutableStorageIfAvailable<R>(
    _ body: (inout UnsafeMutableBufferPointer<Element>) throws -> R
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
  ///     var streets = ["Adams", "Bryant", "Channing", "Douglas", "Evarts"]
  ///     let streetsSlice = streets[2 ..< streets.endIndex]
  ///     print(streetsSlice)
  ///     // Prints "["Channing", "Douglas", "Evarts"]"
  ///
  ///     let index = streetsSlice.firstIndex(of: "Evarts")    // 4
  ///     streets[index!] = "Eustace"
  ///     print(streets[index!])
  ///     // Prints "Eustace"
  ///
  /// - Parameter bounds: A range of the collection's indices. The bounds of
  ///   the range must be valid indices of the collection.
  ///
  /// - Complexity: O(1)
  @inlinable
  public subscript(bounds: Range<Index>) -> Slice<Self> {
    get {
      _failEarlyRangeCheck(bounds, bounds: startIndex..<endIndex)
      return Slice(base: self, bounds: bounds)
    }
    set {
      _writeBackMutableSlice(&self, bounds: bounds, slice: newValue)
    }
  }

  /// Exchanges the values at the specified indices of the collection.
  ///
  /// Both parameters must be valid indices of the collection that are not
  /// equal to `endIndex`. Calling `swapAt(_:_:)` with the same index as both
  /// `i` and `j` has no effect.
  ///
  /// - Parameters:
  ///   - i: The index of the first value to swap.
  ///   - j: The index of the second value to swap.
  ///
  /// - Complexity: O(1)
  @inlinable
  public mutating func swapAt(_ i: Index, _ j: Index) {
    guard i != j else { return }
    let tmp = self[i]
    self[i] = self[j]
    self[j] = tmp
  }
}

//===----------------------------------------------------------------------===//
// moveSubranges(_:to:)
//===----------------------------------------------------------------------===//

extension MutableCollection {
  /// Moves the elements in the given subranges to just before the element at
  /// the specified index.
  ///
  /// This example finds all the uppercase letters in the array and then
  /// moves them to between `"i"` and `"j"`.
  ///
  ///     var letters = Array("ABCdeFGhijkLMNOp")
  ///     let uppercaseRanges = letters.subranges(where: { $0.isUppercase })
  ///     let rangeOfUppercase = letters.moveSubranges(uppercaseRanges, to: 10)
  ///     // String(letters) == "dehiABCFGLMNOjkp"
  ///     // rangeOfUppercase == 4..<13
  ///
  /// - Parameters:
  ///   - subranges: The subranges of the elements to move.
  ///   - insertionPoint: The index to use as the destination of the elements.
  /// - Returns: The new bounds of the moved elements.
  ///
  /// - Complexity: O(*n* log *n*) where *n* is the length of the collection.
  @available(macOS 10.16, iOS 14.0, watchOS 7.0, tvOS 14.0, *)
  @discardableResult
  public mutating func moveSubranges(
    _ subranges: RangeSet<Index>, to insertionPoint: Index
  ) -> Range<Index> {
    let lowerCount = distance(from: startIndex, to: insertionPoint)
    let upperCount = distance(from: insertionPoint, to: endIndex)
    let start = _indexedStablePartition(
      count: lowerCount,
      range: startIndex..<insertionPoint,
      by: { subranges.contains($0) })
    let end = _indexedStablePartition(
      count: upperCount,
      range: insertionPoint..<endIndex,
      by: { !subranges.contains($0) })
    return start..<end
  }
}

//===----------------------------------------------------------------------===//
// _rotate(in:shiftingToStart:)
//===----------------------------------------------------------------------===//

extension MutableCollection {
  /// Rotates the elements of the collection so that the element at `middle`
  /// ends up first.
  ///
  /// - Returns: The new index of the element that was first pre-rotation.
  ///
  /// - Complexity: O(*n*)
  @discardableResult
  internal mutating func _rotate(
    in subrange: Range<Index>,
    shiftingToStart middle: Index
  ) -> Index {
    var m = middle, s = subrange.lowerBound
    let e = subrange.upperBound
    
    // Handle the trivial cases
    if s == m { return e }
    if m == e { return s }
    
    // We have two regions of possibly-unequal length that need to be
    // exchanged.  The return value of this method is going to be the
    // position following that of the element that is currently last
    // (element j).
    //
    //   [a b c d e f g|h i j]   or   [a b c|d e f g h i j]
    //   ^             ^     ^        ^     ^             ^
    //   s             m     e        s     m             e
    //
    var ret = e // start with a known incorrect result.
    while true {
      // Exchange the leading elements of each region (up to the
      // length of the shorter region).
      //
      //   [a b c d e f g|h i j]   or   [a b c|d e f g h i j]
      //    ^^^^^         ^^^^^          ^^^^^ ^^^^^
      //   [h i j d e f g|a b c]   or   [d e f|a b c g h i j]
      //   ^     ^       ^     ^         ^    ^     ^       ^
      //   s    s1       m    m1/e       s   s1/m   m1      e
      //
      let (s1, m1) = _swapNonemptySubrangePrefixes(s..<m, m..<e)
      
      if m1 == e {
        // Left-hand case: we have moved element j into position.  if
        // we haven't already, we can capture the return value which
        // is in s1.
        //
        // Note: the STL breaks the loop into two just to avoid this
        // comparison once the return value is known.  I'm not sure
        // it's a worthwhile optimization, though.
        if ret == e { ret = s1 }
        
        // If both regions were the same size, we're done.
        if s1 == m { break }
      }
      
      // Now we have a smaller problem that is also a rotation, so we
      // can adjust our bounds and repeat.
      //
      //    h i j[d e f g|a b c]   or    d e f[a b c|g h i j]
      //         ^       ^     ^              ^     ^       ^
      //         s       m     e              s     m       e
      s = s1
      if s == m { m = m1 }
    }
    
    return ret
  }
  
  /// Swaps the elements of the two given subranges, up to the upper bound of
  /// the smaller subrange. The returned indices are the ends of the two
  /// ranges that were actually swapped.
  ///
  ///     Input:
  ///     [a b c d e f g h i j k l m n o p]
  ///      ^^^^^^^         ^^^^^^^^^^^^^
  ///      lhs             rhs
  ///
  ///     Output:
  ///     [i j k l e f g h a b c d m n o p]
  ///             ^               ^
  ///             p               q
  ///
  /// - Precondition: !lhs.isEmpty && !rhs.isEmpty
  /// - Postcondition: For returned indices `(p, q)`:
  ///
  ///   - distance(from: lhs.lowerBound, to: p) == distance(from:
  ///     rhs.lowerBound, to: q)
  ///   - p == lhs.upperBound || q == rhs.upperBound
  internal mutating func _swapNonemptySubrangePrefixes(
    _ lhs: Range<Index>, _ rhs: Range<Index>
  ) -> (Index, Index) {
    assert(!lhs.isEmpty)
    assert(!rhs.isEmpty)
    
    var p = lhs.lowerBound
    var q = rhs.lowerBound
    repeat {
      swapAt(p, q)
      formIndex(after: &p)
      formIndex(after: &q)
    } while p != lhs.upperBound && q != rhs.upperBound
    return (p, q)
  }
}

// the legacy swap free function
//
/// Exchanges the values of the two arguments.
///
/// The two arguments must not alias each other. To swap two elements of a
/// mutable collection, use the `swapAt(_:_:)` method of that collection
/// instead of this function.
///
/// - Parameters:
///   - a: The first value to swap.
///   - b: The second value to swap.
@inlinable
public func swap<T>(_ a: inout T, _ b: inout T) {
  // Semantically equivalent to (a, b) = (b, a).
  // Microoptimized to avoid retain/release traffic.
  let p1 = Builtin.addressof(&a)
  let p2 = Builtin.addressof(&b)
  _debugPrecondition(
    p1 != p2,
    "swapping a location with itself is not supported")

  // Take from P1.
  let tmp: T = Builtin.take(p1)
  // Transfer P2 into P1.
  Builtin.initialize(Builtin.take(p2) as T, p1)
  // Initialize P2.
  Builtin.initialize(tmp, p2)
}

