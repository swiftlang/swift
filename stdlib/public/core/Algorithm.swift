//===----------------------------------------------------------------------===//
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

/// Returns the minimum element in `elements`.
///
/// - Requires: `elements` is non-empty. O(`elements.count`).
@available(*, unavailable, message="call the 'minElement()' method on the sequence")
public func minElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(elements: R)
  -> R.Generator.Element {
  fatalError("unavailable function can't be called")
}

/// Returns the maximum element in `elements`.
///
/// - Requires: `elements` is non-empty. O(`elements.count`).
@available(*, unavailable, message="call the 'maxElement()' method on the sequence")
public func maxElement<
     R : SequenceType
       where R.Generator.Element : Comparable>(elements: R)
  -> R.Generator.Element {
  fatalError("unavailable function can't be called")
}

/// Returns the first index where `value` appears in `domain` or `nil` if
/// `value` is not found.
///
/// - Complexity: O(`domain.count`).
@available(*, unavailable, message="call the 'indexOf()' method on the collection")
public func find<
  C: CollectionType where C.Generator.Element : Equatable
>(domain: C, _ value: C.Generator.Element) -> C.Index? {
  fatalError("unavailable function can't be called")
}

/// Returns the lesser of `x` and `y`.
///
/// If `x == y`, returns `x`.
@warn_unused_result
public func min<T : Comparable>(x: T, _ y: T) -> T {
  // In case `x == y` we pick `x`.
  // This preserves any pre-existing order in case `T` has identity,
  // which is important for e.g. the stability of sorting algorithms.
  // `(min(x, y), max(x, y))` should return `(x, y)` in case `x == y`.
  return y < x ? y : x
}

/// Returns the least argument passed.
///
/// If there are multiple equal least arguments, returns the first one.
@warn_unused_result
public func min<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var minValue = min(min(x, y), z)
  // In case `value == minValue`, we pick `minValue`. See min(_:_:).
  for value in rest where value < minValue {
    minValue = value
  }
  return minValue
}

/// Returns the greater of `x` and `y`.
///
/// If `x == y`, returns `y`.
@warn_unused_result
public func max<T : Comparable>(x: T, _ y: T) -> T {
  // In case `x == y`, we pick `y`. See min(_:_:).
  return y >= x ? y : x
}

/// Returns the greatest argument passed.
///
/// If there are multiple equal greatest arguments, returns the last one.
@warn_unused_result
public func max<T : Comparable>(x: T, _ y: T, _ z: T, _ rest: T...) -> T {
  var maxValue = max(max(x, y), z)
  // In case `value == maxValue`, we pick `value`. See min(_:_:).
  for value in rest where value >= maxValue {
    maxValue = value
  }
  return maxValue
}

/// Returns the result of slicing `elements` into sub-sequences that
/// don't contain elements satisfying the predicate `isSeparator`.
///
/// - parameter maxSplit: The maximum number of slices to return, minus 1.
///   If `maxSplit + 1` slices would otherwise be returned, the
///   algorithm stops splitting and returns a suffix of `elements`.
///
/// - parameter allowEmptySlices: If `true`, an empty slice is produced in
///   the result for each pair of consecutive.
@available(*, unavailable, message="Use the split() method instead.")
public func split<S : CollectionType, R : BooleanType>(
  elements: S,
  maxSplit: Int = Int.max,
  allowEmptySlices: Bool = false,
  @noescape isSeparator: (S.Generator.Element) -> R
  ) -> [S.SubSequence] {
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff the initial elements of `s` are equal to `prefix`.
@available(*, unavailable, message="call the 'startsWith()' method on the sequence")
public func startsWith<
  S0 : SequenceType, S1 : SequenceType
  where
    S0.Generator.Element == S1.Generator.Element,
    S0.Generator.Element : Equatable
>(s: S0, _ prefix: S1) -> Bool
{
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff `s` begins with elements equivalent to those of
/// `prefix`, using `isEquivalent` as the equivalence test.
///
/// - Requires: `isEquivalent` is an [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation).
@available(*, unavailable, message="call the 'startsWith()' method on the sequence")
public func startsWith<
  S0 : SequenceType, S1 : SequenceType
  where
    S0.Generator.Element == S1.Generator.Element
>(s: S0, _ prefix: S1,
  @noescape _ isEquivalent: (S1.Generator.Element, S1.Generator.Element) -> Bool)
  -> Bool
{
  fatalError("unavailable function can't be called")
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
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateGenerator` directly.
public struct EnumerateGenerator<
  Base : GeneratorType
> : GeneratorType, SequenceType {
  /// The type of element returned by `next()`.
  public typealias Element = (index: Int, element: Base.Element)
  var base: Base
  var count: Int = 0

  /// Construct from a `Base` generator.
  public init(_ base: Base) {
    self.base = base
  }

  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// - Requires: No preceding call to `self.next()` has returned `nil`.
  public mutating func next() -> Element? {
    guard let b = base.next() else { return nil }
    defer { count += 1 }
    return (index: count, element: b)
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
/// - Note: Idiomatic usage is to call `enumerate` instead of
///   constructing an `EnumerateSequence` directly.
public struct EnumerateSequence<Base : SequenceType> : SequenceType {
  var base: Base

  /// Construct from a `Base` sequence.
  public init(_ base: Base) {
    self.base = base
  }

  /// Returns a *generator* over the elements of this *sequence*.
  ///
  /// - Complexity: O(1).
  public func generate() -> EnumerateGenerator<Base.Generator> {
    return EnumerateGenerator(base.generate())
  }
}

/// Returns a lazy `SequenceType` containing pairs (*n*, *x*), where
/// *n*s are consecutive `Int`s starting at zero, and *x*s are
/// the elements of `base`:
///
///     > for (n, c) in enumerate("Swift".characters) {
///         print("\(n): '\(c)'" )
///       }
///     0: 'S'
///     1: 'w'
///     2: 'i'
///     3: 'f'
///     4: 't'
@available(*, unavailable, message="call the 'enumerate()' method on the sequence")
public func enumerate<Seq : SequenceType>(
  base: Seq
) -> EnumerateSequence<Seq> {
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff `a1` and `a2` contain the same elements in the
/// same order.
@available(*, unavailable, message="call the 'equalElements()' method on the sequence")
public func equal<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element,
    S1.Generator.Element : Equatable
>(a1: S1, _ a2: S2) -> Bool {
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff `a1` and `a2` contain equivalent elements, using
/// `isEquivalent` as the equivalence test.
///
/// - Requires: `isEquivalent` is an [equivalence relation](http://en.wikipedia.org/wiki/Equivalence_relation).
@available(*, unavailable, message="call the 'equalElements()' method on the sequence")
public func equal<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element
>(a1: S1, _ a2: S2,
  @noescape _ isEquivalent: (S1.Generator.Element, S1.Generator.Element) -> Bool)
  -> Bool {
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff `a1` precedes `a2` in a lexicographical ("dictionary")
/// ordering, using "<" as the comparison between elements.
@available(*, unavailable, message="call the 'lexicographicalCompare()' method on the sequence")
public func lexicographicalCompare<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element,
    S1.Generator.Element : Comparable>(
  a1: S1, _ a2: S2) -> Bool {
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff `a1` precedes `a2` in a lexicographical ("dictionary")
/// ordering, using `isOrderedBefore` as the comparison between elements.
///
/// - Requires: `isOrderedBefore` is a
/// [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
/// over the elements of `a1` and `a2`.
@available(*, unavailable, message="call the 'lexicographicalCompare()' method on the sequence")
public func lexicographicalCompare<
    S1 : SequenceType, S2 : SequenceType
  where
    S1.Generator.Element == S2.Generator.Element
>(
  a1: S1, _ a2: S2,
  @noescape isOrderedBefore less: (S1.Generator.Element, S1.Generator.Element)
  -> Bool
) -> Bool {
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff an element in `seq` satisfies `predicate`.
@available(*, unavailable, message="call the 'contains()' method on the sequence")
public func contains<
  S : SequenceType, L : BooleanType
>(seq: S, @noescape _ predicate: (S.Generator.Element) -> L) -> Bool {
  fatalError("unavailable function can't be called")
}

/// Returns `true` iff `x` is in `seq`.
@available(*, unavailable, message="call the 'contains()' method on the sequence")
public func contains<
  S : SequenceType where S.Generator.Element : Equatable
>(seq: S, _ x: S.Generator.Element) -> Bool {
  fatalError("unavailable function can't be called")
}

/// Returns the result of repeatedly calling `combine` with an
/// accumulated value initialized to `initial` and each element of
/// `sequence`, in turn.
@available(*, unavailable, message="call the 'reduce()' method on the sequence")
public func reduce<S : SequenceType, U>(
  sequence: S, _ initial: U, @noescape _ combine: (U, S.Generator.Element) -> U
) -> U {
  fatalError("unavailable function can't be called")
}
