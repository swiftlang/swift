//===--- Algorithms.swift -------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

//===--- Rotate -----------------------------------------------------------===//
//===----------------------------------------------------------------------===//

// In the stdlib, this would simply be MutableCollection
public protocol MutableCollectionAlgorithms : MutableCollection {
  /// Rotates the elements of the collection so that the element
  /// at `middle` ends up first.
  ///
  /// - Returns: The new index of the element that was first
  ///   pre-rotation.
  /// - Complexity: O(*n*)
  @discardableResult
  mutating func rotate(shiftingToStart middle: Index) -> Index

  /// Rotates the elements in `bounds` so that the element
  /// at `middle` ends up first in `bounds`.
  ///
  /// - Returns: The new index of the element that was first
  ///   pre-rotation.
  /// - Complexity: O(*n*)
  @discardableResult
  mutating func rotateSubrange(
    _ bounds: Range<Index>, shiftingToStart middle: Index
  ) -> Index
}

// In the stdlib, this conformance wouldn't be needed
extension Array : MutableCollectionAlgorithms {  }

/// In the stdlib, this would simply be MutableCollection
extension MutableCollectionAlgorithms {
  @inline(__always)
  internal mutating func _swapNonemptySubrangePrefixes(
    _ lhs: Range<Index>, _ rhs: Range<Index>
  ) -> (Index, Index) {
    _sanityCheck(!lhs.isEmpty)
    _sanityCheck(!rhs.isEmpty)
    
    var p = lhs.lowerBound
    var q = rhs.lowerBound
    repeat {
      swap(&self[p], &self[q])
      formIndex(after: &p)
      formIndex(after: &q)
    }
    while p != lhs.upperBound && q != rhs.upperBound
    return (p, q)
  }
  
  @inline(__always)
  internal mutating func _swapSubrangePrefixes(
    _ lhs: Range<Index>, with rhs: Range<Index>
  ) -> (Index, Index) {
    return lhs.isEmpty || rhs.isEmpty
      ? (lhs.lowerBound, rhs.lowerBound)
      : _swapNonemptySubrangePrefixes(lhs, rhs)
  }
  
  /// Rotates the elements of the collection so that the element
  /// at `middle` ends up first.
  ///
  /// - Returns: The new index of the element that was first
  ///   pre-rotation.
  /// - Complexity: O(*n*)
  @discardableResult
  public mutating func rotate(shiftingToStart middle: Index) -> Index {
    return rotateSubrange(startIndex..<endIndex, shiftingToStart: middle)
  }

  /// Rotates the elements in `bounds` so that the element
  /// at `middle` ends up first.
  ///
  /// - Returns: The new index of the element that was first
  ///   pre-rotation.
  /// - Complexity: O(*n*)
  @discardableResult
  public mutating func rotateSubrange(
    _ bounds: Range<Index>, shiftingToStart middle: Index
  ) -> Index {
    return _rotateSubrangeForward(bounds, shiftingToStart: middle)
  }

  // Broken out of the method above for testability purposes
  @discardableResult
  internal mutating func _rotateSubrangeForward(
    _ bounds: Range<Index>, shiftingToStart middle: Index
  ) -> Index {
    var m = middle, s = bounds.lowerBound
    let e = bounds.upperBound

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
}

// We could use Array instead, but this should be much more efficient.
// It should be proposed for addition to the standard library, for
// similar purposes.
public struct CollectionOfTwo<Element> : RandomAccessCollection {
  public typealias Indices = CountableRange<Int>
  
  @inline(__always)
  public init(_ first: Element, _ second: Element) {
    self._elements = (first, second)
  }

  public var startIndex: Int { return 0 }
  public var endIndex: Int   { return 2 }

  public subscript(index: Int) -> Element {
    switch index {
    case 0: return _elements.0
    case 1: return _elements.1
    default: fatalError("Index out of bounds.")
    }
  }
  
  internal let _elements: (Element, Element)
}

extension MutableCollection where Self: BidirectionalCollection {

  // This could be internal, but until we have pinned accessors for
  // slices, every mutating algorithm needs a version that takes
  // indices in order to get performance.
  
  /// Reverses the elements in the given subrange in place.
  ///
  ///     var characters: [Character] = ["^", "C", "a", "f", "é", "$""]
  ///     let r = characters.index(after: characters.startIndex)
  ///             ..< characters.index(before: characters.endIndex)
  ///     characters.reverseSubrange(r)
  ///     print(cafe.characters)
  ///     // Prints "["^", "é", "f", "a", "C", "$"]
  ///
  /// - Complexity: O(*n*), where *n* is the number of elements in the
  ///   subrange.
  public mutating func reverseSubrange(_ bounds: Range<Index>) {
    if bounds.isEmpty { return }
    var f = bounds.lowerBound
    var l = index(before: bounds.upperBound)
    while f < l {
      swap(&self[f], &self[l])
      formIndex(after: &f)
      formIndex(before: &l)
    }
  }
  
  @inline(__always)
  @discardableResult
  internal mutating func _reverseUntil(_ limit: Index) -> (Index, Index) {
    var f = startIndex
    var l = endIndex
    while f != limit && l != limit {
      formIndex(before: &l)
      swap(&self[f], &self[l])
      formIndex(after: &f)
    }
    return (f, l)
  }
  
  /// Rotates the elements of the collection so that the element
  /// at `middle` ends up first.
  ///
  /// - Returns: The new index of the element that was first
  ///   pre-rotation.
  /// - Complexity: O(*n*)
  @discardableResult
  public mutating func rotate(shiftingToStart middle: Index) -> Index {
    // FIXME: this algorithm should be benchmarked on arrays against
    // the forward Collection algorithm above to prove that it's
    // actually faster.  The other one sometimes does more swaps, but
    // has better locality properties.  Similarly, we've omitted a
    // specialization of rotate for RandomAccessCollection that uses
    // cycles per section 11.4 in "From Mathematics to Generic
    // Programming" by A. Stepanov because it has *much* worse
    // locality properties than either of the other implementations.
    // Benchmarks should be performed for that algorithm too, just to
    // be sure.
    reverseSubrange(startIndex..<middle)
    reverseSubrange(middle..<endIndex)
    let (p, q) = _reverseUntil(middle)
    reverseSubrange(p..<q)
    return middle == p  ? q : p
  }
}

/// Returns the greatest common denominator for `m` and `n`.
internal func _gcd(_ m: Int, _ n: Int) -> Int {
  var (m, n) = (m, n)
  while n != 0 {
    let t = m % n
    m = n
    n = t
  }
  return m
}

extension MutableCollection where Self: RandomAccessCollection,
  SubSequence: MutableCollection, SubSequence: RandomAccessCollection {

  /// Rotates elements through a cycle, using `sourceForIndex` to generate
  /// the source index for each movement.
  @inline(__always)
  internal mutating func _rotateCycle(start: Index,
    transform sourceForIndex: @noescape (Index) -> Index)
  {
    let tmp = self[start]
    var (i, j) = (start, sourceForIndex(start))
    while j != start {
      self[i] = self[j]
      i = j
      j = sourceForIndex(j)
    }
    self[i] = tmp
  }
  
  /// Rotates the elements of the collection so that the element
  /// at `middle` ends up first.
  ///
  /// - Returns: The new index of the element that was first
  ///   pre-rotation.
  /// - Complexity: O(*n*)
  @discardableResult
  public mutating func rotateRandomAccess(
    shiftingToStart middle: Index) -> Index
  {
    if middle == startIndex { return endIndex }
    if middle == endIndex { return startIndex }
    
    // The distance to move an element that is moving ->
    let plus = distance(from: startIndex, to: middle)
    // The distance to move an element that is moving <-
    let minus = distance(from: endIndex, to: middle)
    // The new pivot point, aka the destination for the first element
    let pivot = index(startIndex, offsetBy: -minus)
    
    // If the difference moving forward and backward are relative primes,
    // the entire rotation will be completed in one cycle. Otherwise, repeat
    // cycle, moving the start point forward with each cycle.
    let cycles = _gcd(numericCast(plus), -numericCast(minus))
    
    for cycle in 1...cycles {
      _rotateCycle(start: index(startIndex, offsetBy: numericCast(cycle))) {
        index($0, offsetBy: $0 < pivot ? plus : minus)
      }
    }
    return pivot
  }
}

extension Collection where SubSequence : Collection {
  public func rotated(
    shiftingToStart middle: Index
  ) -> FlattenCollection<CollectionOfTwo<SubSequence>> {
    return CollectionOfTwo(
      self[middle..<endIndex], self[startIndex..<middle]
    ).flatten()
  }
}

extension Collection where SubSequence : BidirectionalCollection {
  public func rotated(
    shiftingToStart middle: Index
  ) -> FlattenBidirectionalCollection<CollectionOfTwo<SubSequence>> {
    return CollectionOfTwo(
      self[middle..<endIndex], self[startIndex..<middle]
    ).flatten()
  }
}

extension LazyCollectionProtocol 
  where Elements.SubSequence : Collection, Index == Elements.Index {
  public func rotated(
    shiftingToStart middle: Index
  ) -> LazyCollection<
    FlattenCollection<CollectionOfTwo<Elements.SubSequence>>
  > {
    return elements.rotated(shiftingToStart: middle).lazy
  }
}

extension LazyCollectionProtocol
  where
  Self : BidirectionalCollection,
  Elements : BidirectionalCollection,
  Elements.SubSequence : BidirectionalCollection, Index == Elements.Index {

  public func rotated(
    shiftingToStart middle: Index
  ) -> LazyBidirectionalCollection<
    FlattenBidirectionalCollection<CollectionOfTwo<Elements.SubSequence>>
  > {
    return elements.rotated(shiftingToStart: middle).lazy
  }
}

//===--- Stable Partition -------------------------------------------------===//
//===----------------------------------------------------------------------===//

extension BidirectionalCollection
  where Self : MutableCollectionAlgorithms, 
  SubSequence : BidirectionalCollection, 
  SubSequence.Index == Self.Index {

  @discardableResult
  mutating func stablePartition(
    choosingStartGroupBy p: (Iterator.Element) -> Bool
  ) -> Index {
    return stablyPartitionSubrange(
      startIndex..<endIndex,
      choosingStartGroupBy: p
    )
  }
  
  mutating func stablyPartitionSubrange(
    _ bounds: Range<Index>,
    choosingStartGroupBy p: (Iterator.Element) -> Bool
  ) -> Index {
    return _stablyPartitionSubrange(
      bounds,
      distance: distance(from: bounds.lowerBound, to: bounds.upperBound),
      choosingStartGroupBy: p
    )
  }
  
  mutating func _stablyPartitionSubrange(
    _ bounds: Range<Index>,
    distance n: IndexDistance,
    choosingStartGroupBy p: (Iterator.Element) -> Bool
  ) -> Index {
    assert(n >= 0)
    let (start, end) = (bounds.lowerBound, bounds.upperBound)
    assert(n == distance(from: start, to: end))
    if n == 0 { return start }
    if n == 1 {
      return p(self[start]) ? index(after: start) : start
    }


    // divide and conquer.
    let d = n / numericCast(2)
    let m = index(start, offsetBy: d)
    
    // TTTTTTTTT s FFFFFFF m ?????????????
    let s = _stablyPartitionSubrange(
      start..<m, distance: d, choosingStartGroupBy: p)
    
    // TTTTTTTTT s FFFFFFF m TTTTTTT e FFFFFFFF
	  let e = _stablyPartitionSubrange(
      m..<end, distance: n - d, choosingStartGroupBy: p)
    
    // TTTTTTTTT s TTTTTTT m  FFFFFFF e FFFFFFFF
    return self.rotateSubrange(s..<e, shiftingToStart: m)
  }
}
  
extension Collection {
  func stablyPartitioned(
    choosingStartGroupBy p: (Iterator.Element) -> Bool
  ) -> [Iterator.Element] {
    var a = Array(self)
    a.stablePartition(choosingStartGroupBy: p)
    return a
  }
}

extension LazyCollectionProtocol 
  where Iterator.Element == Elements.Iterator.Element {
  func stablyPartitioned(
    choosingStartGroupBy p: (Iterator.Element) -> Bool
  ) -> LazyCollection<[Iterator.Element]> {
    return elements.stablyPartitioned(choosingStartGroupBy: p).lazy
  }
}

//===--- Tests ------------------------------------------------------------===//
//===----------------------------------------------------------------------===//

var suite = TestSuite("Algorithms")

suite.test("reverseSubrange") {
  for l in 0..<10 {
    let a = Array(0..<l)
    
    for p in a.startIndex...a.endIndex {
      let prefix = a.prefix(upTo: p)
      for q in p...l {
        let suffix = a.suffix(from: q)
        var b = a
        b.reverseSubrange(p..<q)
        expectEqual(
          b,
          Array([prefix, ArraySlice(a[p..<q].reversed()), suffix].flatten()))
        }
    }
  }
}

suite.test("rotate") {
  for l in 0..<11 {
    let a = Array(0..<l)
    
    for p in a.startIndex...a.endIndex {
      let prefix = a.prefix(upTo: p)
      for q in p...l {
        let suffix = a.suffix(from: q)

        for m in p...q {
          var b = a
          let r0 = b._rotateSubrangeForward(p..<q, shiftingToStart: m)
          let rotated = Array([prefix, a[m..<q], a[p..<m], suffix].flatten())
          expectEqual(b, rotated)
          expectEqual(r0, a.index(p, offsetBy: a[m..<q].count))

          b = a
          let r1 = b.rotateSubrange(p..<q, shiftingToStart: m)
          expectEqual(b, rotated)
          expectEqual(r1, r0)
        }
      }
      var b = a
      b.rotate(shiftingToStart: p)
      expectEqual(b, Array(a.rotated(shiftingToStart: p)))
    }
  }
}

suite.test("rotateRandomAccess") {
  for l in 0..<11 {
    let a = Array(0..<l)
    
    for p in a.startIndex...a.endIndex {
      let prefix = a.prefix(upTo: p)
      for q in p...l {
        let suffix = a.suffix(from: q)

        for m in p...q {
          var b = a
          let r0 = b[p..<q].rotateRandomAccess(shiftingToStart: m)
          let rotated = Array([prefix, a[m..<q], a[p..<m], suffix].flatten())
          expectEqual(b, rotated)
          expectEqual(r0, a.index(p, offsetBy: a[m..<q].count))

          b = a
          let r1 = b[p..<q].rotateRandomAccess(shiftingToStart: m)
          expectEqual(b, rotated)
          expectEqual(r1, r0)
        }
      }
      var b = a
      b.rotateRandomAccess(shiftingToStart: p)
      expectEqual(b, Array(a.rotated(shiftingToStart: p)))
    }
  }
}

suite.test("stablePartition") {
  for l in 0..<13 {
    let a = Array(0..<l)
    
    for p in a.startIndex...a.endIndex {
      let prefix = a.prefix(upTo: p)
      for q in p...l {
        let suffix = a.suffix(from: q)

        let subrange = a[p..<q]
        
        for modulus in 1...5 {
          let f = { $0 % modulus == 0 }
          let notf = { !f($0) }
          
          var b = a
          var r = b.stablyPartitionSubrange(p..<q, choosingStartGroupBy: f)
          expectEqual(b.prefix(upTo:p), prefix)
          expectEqual(b.suffix(from:q), suffix)
          expectEqual(b[p..<r], ArraySlice(subrange.filter(f)))
          expectEqual(b[r..<q], ArraySlice(subrange.filter(notf)))

          b = a
          r = b.stablyPartitionSubrange(p..<q, choosingStartGroupBy: notf)
          expectEqual(b.prefix(upTo:p), prefix)
          expectEqual(b.suffix(from:q), suffix)
          expectEqual(b[p..<r], ArraySlice(subrange.filter(notf)))
          expectEqual(b[r..<q], ArraySlice(subrange.filter(f)))
        }
      }
      
      for modulus in 1...5 {
        let f = { $0 % modulus == 0 }
        let notf = { !f($0) }
        var b = a
        var r = b.stablePartition(choosingStartGroupBy: f)
        expectEqual(b.prefix(upTo: r), ArraySlice(a.filter(f)))
        expectEqual(b.suffix(from: r), ArraySlice(a.filter(notf)))

        b = a
        r = b.stablePartition(choosingStartGroupBy: notf)
        expectEqual(b.prefix(upTo: r), ArraySlice(a.filter(notf)))
        expectEqual(b.suffix(from: r), ArraySlice(a.filter(f)))
      }
    }
  }
}

runAllTests()

