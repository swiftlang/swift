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

/// Returns the minimum element in `elements`.  Requires:
/// `elements` is non-empty. O(countElements(elements))
public func minElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(elements: R)
  -> R.Generator.Element {
  var g = elements.generate()
  var result = g.next()!  
  for e in GeneratorSequence(g) {
    if e < result { result = e }
  }
  return result
}

/// Returns the maximum element in `elements`.  Requires:
/// `elements` is non-empty. O(countElements(elements))
public func maxElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(elements: R)
  -> R.Generator.Element {
  var g = elements.generate()
  var result = g.next()!
  for e in GeneratorSequence(g) {
    if e > result { result = e }
  }
  return result
}

// Returns the first index where `value` appears in `domain` or `nil` if
// `domain` doesn't contain `value`. O(countElements(domain))
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

/// Return the lesser of `x` and `y`
public func min<T : Comparable>(x: T, y: T) -> T {
  var r = x
  if y < x {
    r = y
  }
  return r
}

/// Return the least argument passed
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

/// Return the greater of `x` and `y`
public func max<T : Comparable>(x: T, y: T) -> T {
  var r = y
  if y < x {
    r = x
  }
  return r
}

/// Return the greatest argument passed
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

/// Return the result of slicing `elements` into sub-sequences that
/// don't contain elements satisfying the predicate `isSeparator`.
///
/// :param: maxSplit the maximum number of slices to return, minus 1.
/// If `maxSplit + 1` slices would otherwise be returned, the
/// algorithm stops splitting and returns a suffix of `elements`
///
/// :param: allowEmptySlices if true, an empty slice is produced in
/// the result for each pair of consecutive 
public func split<S: Sliceable, R:BooleanType>(
  elements: S, 
  isSeparator: (S.Generator.Element)->R, 
  maxSplit: Int = Int.max,
  allowEmptySlices: Bool = false
  ) -> [S.SubSlice] {

  var result = Array<S.SubSlice>()

  // FIXME: could be simplified pending <rdar://problem/15032945>
  // (ternary operator not resolving some/none)
  var startIndex: Optional<S.Index>
     = allowEmptySlices ? .Some(elements.startIndex) : .None
  var splits = 0

  for j in indices(elements) {
    if isSeparator(elements[j]) {
      if startIndex != nil {
        var i = startIndex!
        result.append(elements[i..<j])
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
    result.append(elements[i..<elements.endIndex])
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

/// Return true iff `s` begins with elements equivalent to those of
/// `prefix`, using `isEquivalent` as the equivalence test.  Requires:
/// `isEquivalent` is an `equivalence relation
/// <http://en.wikipedia.org/wiki/Equivalence_relation>`__
public func startsWith<
  S0: SequenceType, S1: SequenceType
  where
    S0.Generator.Element == S1.Generator.Element
>(s: S0, prefix: S1,
  isEquivalent: (S1.Generator.Element, S1.Generator.Element) -> Bool) -> Bool
{
  var prefixGenerator = prefix.generate()

  for e0 in s {
    var e1 = prefixGenerator.next()
    if e1 == nil { return true }
    if !isEquivalent(e0, e1!) {
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

/// Return true iff `a1` and `a2` contain equivalent elements, using
/// `isEquivalent` as the equivalence test.  Requires: `isEquivalent`
/// is an `equivalence relation
/// <http://en.wikipedia.org/wiki/Equivalence_relation>`__
public func equal<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element
>(a1: S1, a2: S2,
  isEquivalent: (S1.Generator.Element, S1.Generator.Element) -> Bool) -> Bool
{
  var g1 = a1.generate()
  var g2 = a2.generate()
  while true {
    var e1 = g1.next()
    var e2 = g2.next()
    if (e1 != nil) && (e2 != nil) {
      if !isEquivalent(e1!, e2!) {
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
@availability(*, unavailable, renamed="any")
public func contains<
  S: SequenceType, L: BooleanType
>(seq: S, predicate: (S.Generator.Element)->L) -> Bool {
  return any(seq, predicate)
}

/// Return `true` iff an element in `seq` satisfies `predicate`.
public func any<
  S: SequenceType, L: BooleanType
>(seq: S, predicate: (S.Generator.Element)->L) -> Bool {
  for a in seq {
    if predicate(a) {
      return true
    }
  }
  return false
}

/// Return `true` iff all elements in `seq` satisfy `predicate`.
public func all<
  S: SequenceType, L: BooleanType
>(seq: S, predicate: (S.Generator.Element)->L) -> Bool {
  return !any(seq) { !predicate($0) }
}

/// Return `true` iff `x` is in `seq`.
public func contains<
  S: SequenceType where S.Generator.Element: Equatable
>(seq: S, element x: S.Generator.Element) -> Bool {
  return any(seq, { $0 == x })
}

/// Return the result of repeatedly calling `combine` with an
/// accumulated value initialized to `initial` and each element of
/// `sequence`, in turn.
public func reduce<S: SequenceType, U>(
  sequence: S, initial: U, combine: (U, S.Generator.Element)->U
) -> U {
  var result = initial
  for element in sequence {
    result = combine(result, element)
  }
  return result
}
