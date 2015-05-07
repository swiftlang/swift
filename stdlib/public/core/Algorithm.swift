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
/// `elements` is non-empty. O(count(elements))
@availability(*, unavailable, message="call the 'minElement()' method on the sequence")
public func minElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(elements: R)
  -> R.Generator.Element {
  return elements.minElement()!
}

/// Returns the maximum element in `elements`.  Requires:
/// `elements` is non-empty. O(count(elements))
@availability(*, unavailable, message="call the 'maxElement()' method on the sequence")
public func maxElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(elements: R)
  -> R.Generator.Element {
  return elements.maxElement()!
}

/// Returns the first index where `value` appears in `domain` or `nil` if
/// `value` is not found.
///
/// - complexity: O(`count(domain)`)
public func find<
  C: CollectionType where C.Generator.Element : Equatable
>(domain: C, _ value: C.Generator.Element) -> C.Index? {
  // FIXME(prext): remove this function when protocol extensions land.
  return domain._prext_indexOf(value)
}

/// Return the lesser of `x` and `y`
public func min<T : Comparable>(x: T, _ y: T) -> T {
  var r = x
  if y < x {
    r = y
  }
  return r
}

/// Return the least argument passed
public func min<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T {
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
public func max<T : Comparable>(x: T, _ y: T) -> T {
  var r = y
  if y < x {
    r = x
  }
  return r
}

/// Return the greatest argument passed
public func max<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T {
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
/// - parameter maxSplit: the maximum number of slices to return, minus 1.
/// If `maxSplit + 1` slices would otherwise be returned, the
/// algorithm stops splitting and returns a suffix of `elements`
///
/// - parameter allowEmptySlices: if true, an empty slice is produced in
/// the result for each pair of consecutive
public func split<S: Sliceable, R:BooleanType>(
  elements: S,
  maxSplit: Int = Int.max,
  allowEmptySlices: Bool = false,
  @noescape isSeparator: (S.Generator.Element) -> R
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
  case let i?:
    result.append(elements[i..<elements.endIndex])
  default:
    ()
  }
  return result
}

/// Return true iff the the initial elements of `s` are equal to `prefix`.
@availability(*, unavailable, message="call the 'startsWith()' method on the sequence")
public func startsWith<
  S0 : SequenceType, S1 : SequenceType
  where
    S0.Generator.Element == S1.Generator.Element,
    S0.Generator.Element : Equatable
>(s: S0, _ prefix: S1) -> Bool
{
  return s.startsWith(prefix)
}

/// Return true iff `s` begins with elements equivalent to those of
/// `prefix`, using `isEquivalent` as the equivalence test.
///
/// Requires: `isEquivalent` is an [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation)
@availability(*, unavailable, message="call the 'startsWith()' method on the sequence")
public func startsWith<
  S0 : SequenceType, S1 : SequenceType
  where
    S0.Generator.Element == S1.Generator.Element
>(s: S0, _ prefix: S1,
  @noescape _ isEquivalent: (S1.Generator.Element, S1.Generator.Element) -> Bool)
  -> Bool
{
  return s.startsWith(prefix, isEquivalent: isEquivalent)
}

/// The `GeneratorType` for `EnumerateSequence`.  `EnumerateGenerator`
/// wraps a `Base` `GeneratorType` and yields successive `Int` values,
/// starting at zero, along with the elements of the underlying
/// `Base`:
///
///     var g = EnumerateGenerator(["foo", "bar"].generate())
///     g.next() // (0, "foo")
///     g.next() // (1, "bar")
///     g.next() // nil
///
/// - note: idiomatic usage is to call `enumerate` instead of
/// constructing an `EnumerateGenerator` directly.
public struct EnumerateGenerator<
  Base: GeneratorType
> : GeneratorType, SequenceType {
  /// The type of element returned by `next()`.
  public typealias Element = (index: Int, element: Base.Element)
  var base: Base
  var count: Int

  /// Construct from a `Base` generator
  public init(_ base: Base) {
    self.base = base
    count = 0
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: no preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    var b = base.next()
    if b == nil { return .None }
    return .Some((index: count++, element: b!))
  }

  /// A type whose instances can produce the elements of this
  /// sequence, in order.
  public typealias Generator = EnumerateGenerator<Base>

  /// `EnumerateGenerator` is also a `SequenceType`, so it
  /// `generate`s a copy of itself
  public func generate() -> Generator {
    return self
  }
}

/// The `SequenceType` returned by `enumerate()`.  `EnumerateSequence`
/// is a sequence of pairs (*n*, *x*), where *n*s are consecutive
/// `Int`s starting at zero, and *x*s are the elements of a `Base`
/// `SequenceType`:
///
///     var s = EnumerateSequence(["foo", "bar"])
///     Array(s) // [(0, "foo"), (1, "bar")]
///
/// - note: idiomatic usage is to call `enumerate` instead of
/// constructing an `EnumerateSequence` directly.
public struct EnumerateSequence<Base : SequenceType> : SequenceType {
  var base: Base

  /// Construct from a `Base` sequence
  public init(_ base: Base) {
    self.base = base
  }

  /// Return a *generator* over the elements of this *sequence*.
  ///
  /// - complexity: O(1)
  public func generate() -> EnumerateGenerator<Base.Generator> {
    return EnumerateGenerator(base.generate())
  }
}

/// Return a lazy `SequenceType` containing pairs (*n*, *x*), where
/// *n*s are consecutive `Int`s starting at zero, and *x*s are
/// the elements of `base`:
///
///     > for (n, c) in enumerate("Swift".characters) {
///         println("\(n): '\(c)'" )
///       }
///     0: 'S'
///     1: 'w'
///     2: 'i'
///     3: 'f'
///     4: 't'
@availability(*, unavailable, message="call the 'enumerate()' method on the sequence")
public func enumerate<Seq : SequenceType>(
  base: Seq
) -> EnumerateSequence<Seq> {
  return base.enumerate()
}

/// Return `true` iff `a1` and `a2` contain the same elements in the
/// same order.
@availability(*, unavailable, message="call the 'equal()' method on the sequence")
public func equal<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element,
    S1.Generator.Element : Equatable
>(a1: S1, _ a2: S2) -> Bool {
  // FIXME(prext): remove this function when protocol extensions land.
  return a1.elementsEqual(a2)
}

/// Return true iff `a1` and `a2` contain equivalent elements, using
/// `isEquivalent` as the equivalence test.  Requires: `isEquivalent`
/// is an [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation)
@availability(*, unavailable, message="call the 'equal()' method on the sequence")
public func equal<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element
>(a1: S1, _ a2: S2,
  @noescape _ isEquivalent: (S1.Generator.Element, S1.Generator.Element) -> Bool)
  -> Bool {
  // FIXME(prext): remove this function when protocol extensions land.
  return a1.elementsEqual(a2, isEquivalent: isEquivalent)
}

/// Return true iff a1 precedes a2 in a lexicographical ("dictionary")
/// ordering, using "<" as the comparison between elements.
@availability(*, unavailable, message="call the 'lexicographicalCompare()' method on the sequence")
public func lexicographicalCompare<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element,
    S1.Generator.Element : Comparable>(
  a1: S1, _ a2: S2) -> Bool {
  return a1.lexicographicalCompare(a2)
}

/// Return true iff `a1` precedes `a2` in a lexicographical ("dictionary")
/// ordering, using `isOrderedBefore` as the comparison between elements.
///
/// Requires: `isOrderedBefore` is a
/// [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
/// over the elements of `a1` and `a2`.
@availability(*, unavailable, message="call the 'lexicographicalCompare()' method on the sequence")
public func lexicographicalCompare<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element
>(
  a1: S1, _ a2: S2,
  @noescape isOrderedBefore less: (S1.Generator.Element, S1.Generator.Element)
  -> Bool
) -> Bool {
  return a1.lexicographicalCompare(a2, isOrderedBefore: less)
}

/// Return `true` iff an element in `seq` satisfies `predicate`.
@availability(*, unavailable, message="call the 'contains()' method on the sequence")
public func contains<
  S : SequenceType, L : BooleanType
>(seq: S, @noescape _ predicate: (S.Generator.Element) -> L) -> Bool {
  return seq.contains({ predicate($0).boolValue })
}

/// Return `true` iff `x` is in `seq`.
@availability(*, unavailable, message="call the 'contains()' method on the sequence")
public func contains<
  S : SequenceType where S.Generator.Element : Equatable
>(seq: S, _ x: S.Generator.Element) -> Bool {
  return seq.contains(x)
}

/// Return the result of repeatedly calling `combine` with an
/// accumulated value initialized to `initial` and each element of
/// `sequence`, in turn.
@availability(*, unavailable, message="call the 'reduce()' method on the sequence")
public func reduce<S : SequenceType, U>(
  sequence: S, _ initial: U, @noescape _ combine: (U, S.Generator.Element) -> U
) -> U {
  return sequence.reduce(initial, combine: combine)
}
