//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public func minElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(range: R)
  -> R.Generator.Element {
  var g = range.generate()
  var result = g.next()!  
  for e in GeneratorSequence(g) {
    if e < result { result = e }
  }
  return result
}

public func maxElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(range: R)
  -> R.Generator.Element {
  var g = range.generate()
  var result = g.next()!
  for e in GeneratorSequence(g) {
    if e > result { result = e }
  }
  return result
}

// Returns the first index where value appears in domain or nil if
// domain doesn't contain the value. O(countElements(domain))
public func find<
  C: CollectionType where C.Generator.Element : Equatable
>(domain: C, value: C.Generator.Element) -> C.Index? {
  for i in indices(domain) {
    if domain[i] == value {
      return i
    }
  }
  return nil
}

func _insertionSort<
  C: MutableCollectionType where C.Index: BidirectionalIndexType
>(
  inout elements: C,
  range: Range<C.Index>,
  inout less: (C.Generator.Element, C.Generator.Element)->Bool
) {
  if !range.isEmpty {
    let start = range.startIndex

    // Keep track of the end of the initial sequence of sorted
    // elements.  
    var sortedEnd = start

    // One element is trivially already-sorted, thus pre-increment
    // Continue until the sorted elements cover the whole sequence
    while (++sortedEnd != range.endIndex) {
      // get the first unsorted element
      var x: C.Generator.Element = elements[sortedEnd]

      // Look backwards for x's position in the sorted sequence,
      // moving elements forward to make room.
      var i = sortedEnd
      do {
        let predecessor: C.Generator.Element = elements[i.predecessor()]
        
        // if x doesn't belong before y, we've found its position
        if !less(x, predecessor) {
          break
        }
        
        // Move y forward 
        elements[i] = predecessor
      }
      while --i != start
      
      if i != sortedEnd {
        // Plop x into position
        elements[i] = x
      }
    }
  }
}

/// Partition a range into two partially sorted regions and return
/// the index of the pivot:
/// [start..idx), pivot ,[idx..end)
public func partition<
  C: MutableCollectionType where C.Index: RandomAccessIndexType
>(
  inout elements: C,
  range: Range<C.Index>,
  var less: (C.Generator.Element, C.Generator.Element)->Bool
) -> C.Index {
  return _partition(&elements, range, &less)
}

func _partition<
  C: MutableCollectionType where C.Index: RandomAccessIndexType
>(
  inout elements: C,
  range: Range<C.Index>,
  inout less: (C.Generator.Element, C.Generator.Element)->Bool
) -> C.Index {
  var lo = range.startIndex
  var hi = range.endIndex

  if lo == hi {
    return lo
  }
  
  // The first element is the pivot.
  let pivot = elements[range.startIndex]

  // Loop invariants:
  // * lo < hi
  // * elements[i] < pivot, for i in range.startIndex+1..lo
  // * pivot <= elements[i] for i in hi..range.endIndex
  
Loop: while true {
  FindLo: do {
      while ++lo != hi {
        if !less(elements[lo], pivot) { break FindLo }
      }
      break Loop
    } while false

  FindHi: do {
      while --hi != lo {
        if less(elements[hi], pivot) { break FindHi }
      }
      break Loop
    } while false
    
    swap(&elements[lo], &elements[hi])
  }
  
  // swap the pivot into place
  swap(&elements[--lo], &elements[range.startIndex])
  return lo
}


func _quickSort<
  C: MutableCollectionType where C.Index: RandomAccessIndexType
>(
  inout elements: C,
  range: Range<C.Index>,
  less: (C.Generator.Element, C.Generator.Element)->Bool
) {
  var comp = less
  _quickSortImpl(&elements, range, &comp)
}

func _quickSortImpl<
  C: MutableCollectionType where C.Index: RandomAccessIndexType
>(
  inout elements: C,
  range: Range<C.Index>,
  inout less: (C.Generator.Element, C.Generator.Element)->Bool
) {

  // Insertion sort is better at handling smaller regions.
  let cnt = count(range)
  if cnt < 20 {
    _insertionSort(&elements, range, &less)
    return
  }

   // Partition and sort.
  let part_idx : C.Index = _partition(&elements, range, &less)
  _quickSortImpl(&elements, range.startIndex..<part_idx, &less);
  _quickSortImpl(&elements, (part_idx.successor())..<range.endIndex, &less);
}

struct Less<T: Comparable> {
  static func compare(x: T, _ y: T) -> Bool {
    return x < y
  }
}

/// Sort `collection` in-place according to `predicate`.  Requires:
/// `predicate` induces a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sort<
  C: MutableCollectionType where C.Index: RandomAccessIndexType
>(
  inout collection: C,
  predicate: (C.Generator.Element, C.Generator.Element) -> Bool
) {
  _quickSort(&collection, indices(collection), predicate)
}

/// Sort `collection` in-place.  Requires:
/// `<` induces a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sort<
  C: MutableCollectionType 
    where C.Index: RandomAccessIndexType, C.Generator.Element: Comparable
>(
  inout collection: C
) {
  _quickSort(&collection, indices(collection))
}

/// Sort `array` in-place according to `predicate`.  Requires:
/// `predicate` induces a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sort<T>(inout array: [T], predicate: (T, T) -> Bool) {
  return array.withUnsafeMutableBufferPointer {
    a in sort(&a, predicate)
    return
  }
}

// The functions below are a copy of the functions above except that
// they don't accept a predicate and they are hardcoded to use the less-than
// comparator.

/// Sort `array` in-place.  Requires:
/// `<` induces a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sort<T : Comparable>(inout array: [T]) {
  return array.withUnsafeMutableBufferPointer {
    a in sort(&a)
    return
  }
}

/// Return an `Array` containing the elements of `source` sorted
/// according to `predicate`.  Requires: `predicate` induces a `strict
/// weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sorted<
  C: MutableCollectionType where C.Index: RandomAccessIndexType
>(
  source: C,
  predicate: (C.Generator.Element, C.Generator.Element) -> Bool
) -> C {
  var result = source
  sort(&result, predicate)
  return result
}

/// Return an `Array` containing the elements of `source`, sorted.
/// Requires: `<` induces a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sorted<
  C: MutableCollectionType 
    where C.Generator.Element: Comparable, C.Index: RandomAccessIndexType
>(source: C) -> C {
  var result = source
  sort(&result)
  return result
}

/// Return an `Array` containing the elements of `source` sorted
/// according to `predicate`.  Requires: `predicate` induces a `strict
/// weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sorted<
  S: SequenceType
>(
  source: S,
  predicate: (S.Generator.Element, S.Generator.Element) -> Bool
) -> [S.Generator.Element] {
  var result = Array(source)
  sort(&result, predicate)
  return result
}

/// Return an `Array` containing the elements of `source`, sorted.
/// Requires: `<` induces a `strict weak ordering
/// <http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings>`__
/// over the elements.
public func sorted<
  S: SequenceType 
    where S.Generator.Element: Comparable
>(
  source: S
) -> [S.Generator.Element] {
  var result = Array(source)
  sort(&result)
  return result
}

func _insertionSort<
  C: MutableCollectionType where C.Index: RandomAccessIndexType,
  C.Generator.Element: Comparable>(
  inout elements: C,
  range: Range<C.Index>) {

  if !range.isEmpty {
    let start = range.startIndex

    // Keep track of the end of the initial sequence of sorted
    // elements.
    var sortedEnd = start

    // One element is trivially already-sorted, thus pre-increment
    // Continue until the sorted elements cover the whole sequence
    while (++sortedEnd != range.endIndex) {
      // get the first unsorted element
      var x: C.Generator.Element = elements[sortedEnd]

      // Look backwards for x's position in the sorted sequence,
      // moving elements forward to make room.
      var i = sortedEnd
      do {
        let predecessor: C.Generator.Element = elements[i.predecessor()]

        // if x doesn't belong before y, we've found its position
        if !Less.compare(x, predecessor) {
          break
        }

        // Move y forward
        elements[i] = predecessor
      }
      while --i != start

      if i != sortedEnd {
        // Plop x into position
        elements[i] = x
      }
    }
  }
}

/// Partition a range into two partially sorted regions and return
/// the index of the pivot:
/// [start..idx), pivot ,[idx..end)
public func partition<
  C: MutableCollectionType where C.Generator.Element: Comparable
, C.Index: RandomAccessIndexType
>(
  inout elements: C,
  range: Range<C.Index>) -> C.Index {
  
  var lo = range.startIndex
  var hi = range.endIndex

  if lo == hi {
    return lo
  }
  
  // The first element is the pivot.
  let pivot = elements[range.startIndex]

  // Loop invariants:
  // * lo < hi
  // * elements[i] < pivot, for i in range.startIndex+1..lo
  // * pivot <= elements[i] for i in hi..range.endIndex
  
Loop: while true {
  FindLo: do {
      while ++lo != hi {
        if !(elements[lo] < pivot) { break FindLo }
      }
      break Loop
    } while false

  FindHi: do {
      while --hi != lo {
        if (elements[hi] < pivot) { break FindHi }
      }
      break Loop
    } while false
    
    swap(&elements[lo], &elements[hi])
  }
  
  // swap the pivot into place
  swap(&elements[--lo], &elements[range.startIndex])
  return lo
}

func _quickSort<
  C: MutableCollectionType
    where C.Generator.Element: Comparable, C.Index: RandomAccessIndexType
>(
  inout elements: C,
  range: Range<C.Index>) {
  _quickSortImpl(&elements, range)
}

func _quickSortImpl<
  C: MutableCollectionType
    where C.Generator.Element: Comparable, C.Index: RandomAccessIndexType
>(
  inout elements: C, range: Range<C.Index>
) {
  // Insertion sort is better at handling smaller regions.
  let cnt = count(range)
  if cnt < 20 {
    _insertionSort(&elements, range)
    return
  }
   // Partition and sort.
  let part_idx : C.Index = partition(&elements, range)
  _quickSortImpl(&elements, range.startIndex..<part_idx);
  _quickSortImpl(&elements, (part_idx.successor())..<range.endIndex);
}
//// End of non-predicate sort functions.

/// Exchange the values of `a` and `b`
public func swap<T>(inout a : T, inout b : T) {
  // Semantically equivalent to (a, b) = (b, a).
  // Microoptimized to avoid retain/release traffic.
  let p1 = Builtin.addressof(&a)
  let p2 = Builtin.addressof(&b)
  
  // Take from P1.
  let tmp : T = Builtin.take(p1)
  // Transfer P2 into P1.
  Builtin.initialize(Builtin.take(p2) as T, p1)
  // Initialize P2.
  Builtin.initialize(tmp, p2)
}


public func min<T : Comparable>(x: T, y: T) -> T {
  var r = x
  if y < x {
    r = y
  }
  return r
}

public func min<T : Comparable>(x: T, y: T, z: T, rest: T...) -> T {
  var r = x
  if y < x {
    r = y
  }
  if z < r {
    r = z
  }
  for t in rest {
    if t < r {
      r = t
    }
  }
  return r
}

public func max<T : Comparable>(x: T, y: T) -> T {
  var r = y
  if y < x {
    r = x
  }
  return r
}

public func max<T : Comparable>(x: T, y: T, z: T, rest: T...) -> T {
  var r = y
  if y < x {
    r = x
  }
  if r < z {
    r = z
  }
  for t in rest {
    if t >= r {
      r = t
    }
  }
  return r
}

public func split<Seq: Sliceable, R:BooleanType>(
  seq: Seq, 
  isSeparator: (Seq.Generator.Element)->R, 
  maxSplit: Int = Int.max,
  allowEmptySlices: Bool = false
  ) -> [Seq.SubSlice] {

  var result = Array<Seq.SubSlice>()

  // FIXME: could be simplified pending <rdar://problem/15032945>
  // (ternary operator not resolving some/none)
  var startIndex: Optional<Seq.Index>
     = allowEmptySlices ? .Some(seq.startIndex) : .None
  var splits = 0

  for j in indices(seq) {
    if isSeparator(seq[j]) {
      if startIndex != nil {
        var i = startIndex!
        result.append(seq[i..<j])
        startIndex = .Some(j.successor())
        if ++splits >= maxSplit {
          break
        }
        if !allowEmptySlices {
          startIndex = .None
        }
      }
    }
    else {
      if startIndex == nil {
        startIndex = .Some(j)
      }
    }
  }

  switch startIndex {
  case .Some(var i):
    result.append(seq[i..<seq.endIndex])
  default:
    ()
  }
  return result
}

/// Return true iff the the initial elements of `s` are equal to `prefix`.
public func startsWith<
  S0: SequenceType, S1: SequenceType
  where
    S0.Generator.Element == S1.Generator.Element,
    S0.Generator.Element : Equatable
>(s: S0, prefix: S1) -> Bool
{
  var prefixGenerator = prefix.generate()

  for e0 in s {
    var e1 = prefixGenerator.next()
    if e1 == nil { return true }
    if e0 != e1! {
      return false
    }
  }
  return prefixGenerator.next() != nil ? false : true
}

/// Return true iff the the initial elements of `s` are equal to `prefix`,
/// using `pred` as equality `==` comparison.
public func startsWith<
  S0: SequenceType, S1: SequenceType
  where
    S0.Generator.Element == S1.Generator.Element
>(s: S0, prefix: S1,
  predicate: (S1.Generator.Element, S1.Generator.Element) -> Bool) -> Bool
{
  var prefixGenerator = prefix.generate()

  for e0 in s {
    var e1 = prefixGenerator.next()
    if e1 == nil { return true }
    if !predicate(e0, e1!) {
      return false
    }
  }
  return prefixGenerator.next() != nil ? false : true
}

public struct EnumerateGenerator<
  Base: GeneratorType
> : GeneratorType, SequenceType {
  public typealias Element = (index: Int, element: Base.Element)
  var base: Base
  var count: Int

  init(_ base: Base) {
    self.base = base
    count = 0
  }

  public mutating func next() -> Element? {
    var b = base.next()
    if b == nil { return .None }
    return .Some((index: count++, element: b!))
  }

  // Every GeneratorType is also a single-pass SequenceType
  public typealias Generator = EnumerateGenerator<Base>
  public func generate() -> Generator {
    return self
  }
}

public func enumerate<Seq : SequenceType>(
  seq: Seq
) -> EnumerateGenerator<Seq.Generator> {
  return EnumerateGenerator(seq.generate())
}


/// Return true iff `a1` and `a2` contain the same elements.
public func equal<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element,
    S1.Generator.Element : Equatable
>(a1: S1, a2: S2) -> Bool
{
  var g1 = a1.generate()
  var g2 = a2.generate()
  while true {
    var e1 = g1.next()
    var e2 = g2.next()
    if (e1 != nil) && (e2 != nil) {
      if e1! != e2! {
        return false
      }
    }
    else {
      return (e1 == nil) == (e2 == nil)
    }
  }
}

/// Return true iff `a1` and `a2` contain the same elements, using
/// `pred` as equality `==` comparison.
public func equal<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element
>(a1: S1, a2: S2,
  predicate: (S1.Generator.Element, S1.Generator.Element) -> Bool) -> Bool
{
  var g1 = a1.generate()
  var g2 = a2.generate()
  while true {
    var e1 = g1.next()
    var e2 = g2.next()
    if (e1 != nil) && (e2 != nil) {
      if !predicate(e1!, e2!) {
        return false
      }
    }
    else {
      return (e1 == nil) == (e2 == nil)
    }
  }
}

/// Return true iff a1 precedes a2 in a lexicographical ("dictionary")
/// ordering, using "<" as the comparison between elements.
public func lexicographicalCompare<
    S1 : SequenceType, S2 : SequenceType
  where 
    S1.Generator.Element == S2.Generator.Element,
    S1.Generator.Element : Comparable>(
  a1: S1, a2: S2) -> Bool {
  var g1 = a1.generate()
  var g2 = a2.generate()
  while true {
    var e1_ = g1.next()
    var e2_ = g2.next()
    if let e1 = e1_ {
      if let e2 = e2_ {
        if e1 < e2 {
          return true
        }
        if e2 < e1 {
          return false
        }
        continue // equivalent
      }
      return false
    }

    return e2_ != nil
  }
}

/// Return true iff `a1` precedes `a2` in a lexicographical ("dictionary")
/// ordering, using `less` as the comparison between elements.
public func lexicographicalCompare<
    S1 : SequenceType, S2 : SequenceType
  where 
    S1.Generator.Element == S2.Generator.Element
>(
  a1: S1, a2: S2,
  less: (S1.Generator.Element,S1.Generator.Element)->Bool
) -> Bool {
  var g1 = a1.generate()
  var g2 = a2.generate()
  while true {
    var e1_ = g1.next()
    var e2_ = g2.next()
    if let e1 = e1_ {
      if let e2 = e2_ {
        if less(e1, e2) {
          return true
        }
        if less(e2, e1) {
          return false
        }
        continue // equivalent
      }
      return false
    }
    switch e2_ {
    case .Some(_): return true
    case .None: return false
    }
  }
}

/// Return `true` iff an element in `seq` satisfies `predicate`.
public func contains<
  S: SequenceType, L: BooleanType
>(seq: S, predicate: (S.Generator.Element)->L) -> Bool {
  for a in seq {
    if predicate(a) {
      return true
    }
  }
  return false
}

/// Return `true` iff `x` is in `seq`.
public func contains<
  S: SequenceType where S.Generator.Element: Equatable
>(seq: S, x: S.Generator.Element) -> Bool {
  return contains(seq, { $0 == x })
}

public func reduce<S: SequenceType, U>(
  sequence: S, initial: U, combine: (U, S.Generator.Element)->U
) -> U {
  var result = initial
  for element in sequence {
    result = combine(result, element)
  }
  return result
}
