//===--- Tuple.swift ------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

/// Returns a Boolean value indicating whether the corresponding components of
/// two tuples are equal.
///
/// All arity zero tuples are equal.
///
/// - Parameters:
///   - lhs: An empty tuple.
///   - rhs: An empty tuple.
@inlinable // trivial-implementation
public func ==(lhs: (), rhs: ()) -> Bool {
  return true
}

/// Returns a Boolean value indicating whether any corresponding components of
/// the two tuples are not equal.
///
/// All arity zero tuples are equal.
///
/// - Parameters:
///   - lhs: An empty tuple.
///   - rhs: An empty tuple.
@inlinable // trivial-implementation
public func !=(lhs: (), rhs: ()) -> Bool {
    return false
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before the second in a lexicographical ordering.
///
/// An arity zero tuple is never strictly before another arity zero tuple in a
/// lexicographical ordering.
///
/// - Parameters:
///   - lhs: An empty tuple.
///   - rhs: An empty tuple.
@inlinable // trivial-implementation
public func <(lhs: (), rhs: ()) -> Bool {
    return false
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before or the same as the second in a lexicographical ordering.
///
/// An arity zero tuple is always before or the same as another arity zero tuple
/// in a lexicographical ordering.
///
/// - Parameters:
///   - lhs: An empty tuple.
///   - rhs: An empty tuple.
@inlinable // trivial-implementation
public func <=(lhs: (), rhs: ()) -> Bool {
    return true
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after the second in a lexicographical ordering.
///
/// An arity zero tuple is never strictly after another arity zero tuple in a
/// lexicographical ordering.
///
/// - Parameters:
///   - lhs: An empty tuple.
///   - rhs: An empty tuple.
@inlinable // trivial-implementation
public func >(lhs: (), rhs: ()) -> Bool {
    return false
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after or the same as the second in a lexicographical ordering.
///
/// An arity zero tuple is always after or the same as another arity zero tuple
/// in a lexicographical ordering.
///
/// - Parameters:
///   - lhs: An empty tuple.
///   - rhs: An empty tuple.
@inlinable // trivial-implementation
public func >=(lhs: (), rhs: ()) -> Bool {
    return true
}

/// Returns a Boolean value indicating whether the corresponding components of
/// two tuples are equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 2
/// components:
///
///     let a = ("a", 1)
///     let b = ("a", 1)
///     print(a == b)
///     // Prints "true"
///
///     let c = ("a", 2)
///     print(a == c)
///     // Prints "false"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func == <A: Equatable, B: Equatable>(lhs: (A, B), rhs: (A, B)) -> Bool {
  guard lhs.0 == rhs.0 else { return false }
  /*tail*/ return lhs.1 == rhs.1
}

/// Returns a Boolean value indicating whether any corresponding components of
/// the two tuples are not equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 2
/// components:
///
///     let a = ("a", 1)
///     let b = ("a", 1)
///     print(a != b)
///     // Prints "false"
///
///     let c = ("a", 2)
///     print(a != c)
///     // Prints "true"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func != <A: Equatable, B: Equatable>(lhs: (A, B), rhs: (A, B)) -> Bool {
  guard lhs.0 == rhs.0 else { return true }
  /*tail*/ return lhs.1 != rhs.1
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) < (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func < <A: Comparable, B: Comparable>(lhs: (A, B), rhs: (A, B)) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 < rhs.0 }
  /*tail*/ return lhs.1 < rhs.1
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before or the same as the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) <= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func <= <A: Comparable, B: Comparable>(
  lhs: (A, B),
  rhs: (A, B)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 <= rhs.0 }
  /*tail*/ return lhs.1 <= rhs.1
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) > (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func > <A: Comparable, B: Comparable>(lhs: (A, B), rhs: (A, B)) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 > rhs.0 }
  /*tail*/ return lhs.1 > rhs.1
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after or the same as the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) >= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func >= <A: Comparable, B: Comparable>(
  lhs: (A, B),
  rhs: (A, B)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 >= rhs.0 }
  /*tail*/ return lhs.1 >= rhs.1
}

/// Returns a Boolean value indicating whether the corresponding components of
/// two tuples are equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 3
/// components:
///
///     let a = ("a", 1, 2)
///     let b = ("a", 1, 2)
///     print(a == b)
///     // Prints "true"
///
///     let c = ("a", 1, 3)
///     print(a == c)
///     // Prints "false"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func == <A: Equatable, B: Equatable, C: Equatable>(
  lhs: (A, B, C),
  rhs: (A, B, C)
) -> Bool {
  guard lhs.0 == rhs.0 else { return false }
  /*tail*/ return (lhs.1, lhs.2) == (rhs.1, rhs.2)
}

/// Returns a Boolean value indicating whether any corresponding components of
/// the two tuples are not equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 3
/// components:
///
///     let a = ("a", 1, 2)
///     let b = ("a", 1, 2)
///     print(a != b)
///     // Prints "false"
///
///     let c = ("a", 1, 3)
///     print(a != c)
///     // Prints "true"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func != <A: Equatable, B: Equatable, C: Equatable>(
  lhs: (A, B, C),
  rhs: (A, B, C)
) -> Bool {
  guard lhs.0 == rhs.0 else { return true }
  /*tail*/ return (lhs.1, lhs.2) != (rhs.1, rhs.2)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) < (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func < <A: Comparable, B: Comparable, C: Comparable>(
  lhs: (A, B, C),
  rhs: (A, B, C)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 < rhs.0 }
  /*tail*/ return (lhs.1, lhs.2) < (rhs.1, rhs.2)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before or the same as the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) <= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func <= <A: Comparable, B: Comparable, C: Comparable>(
  lhs: (A, B, C),
  rhs: (A, B, C)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 <= rhs.0 }
  /*tail*/ return (lhs.1, lhs.2) <= (rhs.1, rhs.2)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) > (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func > <A: Comparable, B: Comparable, C: Comparable>(
  lhs: (A, B, C),
  rhs: (A, B, C)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 > rhs.0 }
  /*tail*/ return (lhs.1, lhs.2) > (rhs.1, rhs.2)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after or the same as the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) >= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func >= <A: Comparable, B: Comparable, C: Comparable>(
  lhs: (A, B, C),
  rhs: (A, B, C)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 >= rhs.0 }
  /*tail*/ return (lhs.1, lhs.2) >= (rhs.1, rhs.2)
}

/// Returns a Boolean value indicating whether the corresponding components of
/// two tuples are equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 4
/// components:
///
///     let a = ("a", 1, 2, 3)
///     let b = ("a", 1, 2, 3)
///     print(a == b)
///     // Prints "true"
///
///     let c = ("a", 1, 2, 4)
///     print(a == c)
///     // Prints "false"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func == <A: Equatable, B: Equatable, C: Equatable, D: Equatable>(
  lhs: (A, B, C, D),
  rhs: (A, B, C, D)
) -> Bool {
  guard lhs.0 == rhs.0 else { return false }
  /*tail*/ return (lhs.1, lhs.2, lhs.3) == (rhs.1, rhs.2, rhs.3)
}

/// Returns a Boolean value indicating whether any corresponding components of
/// the two tuples are not equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 4
/// components:
///
///     let a = ("a", 1, 2, 3)
///     let b = ("a", 1, 2, 3)
///     print(a != b)
///     // Prints "false"
///
///     let c = ("a", 1, 2, 4)
///     print(a != c)
///     // Prints "true"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func != <A: Equatable, B: Equatable, C: Equatable, D: Equatable>(
  lhs: (A, B, C, D),
  rhs: (A, B, C, D)
) -> Bool {
  guard lhs.0 == rhs.0 else { return true }
  /*tail*/ return (lhs.1, lhs.2, lhs.3) != (rhs.1, rhs.2, rhs.3)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) < (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func < <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(
  lhs: (A, B, C, D),
  rhs: (A, B, C, D)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 < rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3) < (rhs.1, rhs.2, rhs.3)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before or the same as the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) <= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func <= <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(
  lhs: (A, B, C, D),
  rhs: (A, B, C, D)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 <= rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3) <= (rhs.1, rhs.2, rhs.3)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) > (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func > <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(
  lhs: (A, B, C, D),
  rhs: (A, B, C, D)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 > rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3) > (rhs.1, rhs.2, rhs.3)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after or the same as the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) >= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func >= <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(
  lhs: (A, B, C, D),
  rhs: (A, B, C, D)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 >= rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3) >= (rhs.1, rhs.2, rhs.3)
}

/// Returns a Boolean value indicating whether the corresponding components of
/// two tuples are equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 5
/// components:
///
///     let a = ("a", 1, 2, 3, 4)
///     let b = ("a", 1, 2, 3, 4)
///     print(a == b)
///     // Prints "true"
///
///     let c = ("a", 1, 2, 3, 5)
///     print(a == c)
///     // Prints "false"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func == <
  A: Equatable,
  B: Equatable,
  C: Equatable,
  D: Equatable,
  E: Equatable
>(
  lhs: (A,B,C,D,E),
  rhs: (A,B,C,D,E)
) -> Bool {
  guard lhs.0 == rhs.0 else { return false }
  /*tail*/ return (lhs.1, lhs.2, lhs.3, lhs.4) == (rhs.1, rhs.2, rhs.3, rhs.4)
}

/// Returns a Boolean value indicating whether any corresponding components of
/// the two tuples are not equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 5
/// components:
///
///     let a = ("a", 1, 2, 3, 4)
///     let b = ("a", 1, 2, 3, 4)
///     print(a != b)
///     // Prints "false"
///
///     let c = ("a", 1, 2, 3, 5)
///     print(a != c)
///     // Prints "true"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func != <
  A: Equatable,
  B: Equatable,
  C: Equatable,
  D: Equatable,
  E: Equatable
>(
  lhs: (A, B, C, D, E),
  rhs: (A, B, C, D, E)
) -> Bool {
  guard lhs.0 == rhs.0 else { return true }
  /*tail*/ return (
    lhs.1, lhs.2, lhs.3, lhs.4
  ) != (
    rhs.1, rhs.2, rhs.3, rhs.4
  )
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) < (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func < <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable
>(
  lhs: (A, B, C, D, E),
  rhs: (A, B, C, D, E)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 < rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3, lhs.4) < (rhs.1, rhs.2, rhs.3, rhs.4)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before or the same as the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) <= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func <= <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable
>(
  lhs: (A, B, C, D, E),
  rhs: (A, B, C, D, E)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 <= rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3, lhs.4) <= (rhs.1, rhs.2, rhs.3, rhs.4)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) > (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func > <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable
>(
  lhs: (A, B, C, D, E),
  rhs: (A, B, C, D, E)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 > rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3, lhs.4) > (rhs.1, rhs.2, rhs.3, rhs.4)
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after or the same as the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) >= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func >= <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable
>(
  lhs: (A, B, C, D, E),
  rhs: (A, B, C, D, E)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 >= rhs.0 }
  /*tail*/ return (lhs.1, lhs.2, lhs.3, lhs.4) >= (rhs.1, rhs.2, rhs.3, rhs.4)
}

/// Returns a Boolean value indicating whether the corresponding components of
/// two tuples are equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 6
/// components:
///
///     let a = ("a", 1, 2, 3, 4, 5)
///     let b = ("a", 1, 2, 3, 4, 5)
///     print(a == b)
///     // Prints "true"
///
///     let c = ("a", 1, 2, 3, 4, 6)
///     print(a == c)
///     // Prints "false"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func == <
  A: Equatable,
  B: Equatable,
  C: Equatable,
  D: Equatable,
  E: Equatable,
  F: Equatable
>(
  lhs: (A, B, C, D, E, F),
  rhs: (A, B, C, D, E, F)
) -> Bool {
  guard lhs.0 == rhs.0 else { return false }
  /*tail*/ return (
    lhs.1, lhs.2, lhs.3, lhs.4, lhs.5
  ) == (
    rhs.1, rhs.2, rhs.3, rhs.4, rhs.5
  )
}

/// Returns a Boolean value indicating whether any corresponding components of
/// the two tuples are not equal.
///
/// For two tuples to compare as equal, each corresponding pair of components
/// must be equal. The following example compares tuples made up of 6
/// components:
///
///     let a = ("a", 1, 2, 3, 4, 5)
///     let b = ("a", 1, 2, 3, 4, 5)
///     print(a != b)
///     // Prints "false"
///
///     let c = ("a", 1, 2, 3, 4, 6)
///     print(a != c)
///     // Prints "true"
///
/// - Parameters:
///   - lhs: A tuple of `Equatable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func != <
  A: Equatable,
  B: Equatable,
  C: Equatable,
  D: Equatable,
  E: Equatable,
  F: Equatable
>(
  lhs: (A, B, C, D, E, F),
  rhs: (A, B, C, D, E, F)
) -> Bool {
  guard lhs.0 == rhs.0 else { return true }
  /*tail*/ return (
    lhs.1, lhs.2, lhs.3, lhs.4, lhs.5
  ) != (
    rhs.1, rhs.2, rhs.3, rhs.4, rhs.5
  )
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) < (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func < <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable,
  F: Comparable
>(
  lhs: (A, B, C, D, E, F),
  rhs: (A, B, C, D, E, F)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 < rhs.0 }
  /*tail*/ return (
    lhs.1, lhs.2, lhs.3, lhs.4, lhs.5
  ) < (
    rhs.1, rhs.2, rhs.3, rhs.4, rhs.5
  )
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// before or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is before or the same as the second tuple if and only if
/// `a1 < b1` or (`a1 == b1` and
/// `(a2, ..., aN) <= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func <= <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable,
  F: Comparable
>(
  lhs: (A, B, C, D, E, F),
  rhs: (A, B, C, D, E, F)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 <= rhs.0 }
  /*tail*/ return (
    lhs.1, lhs.2, lhs.3, lhs.4, lhs.5
  ) <= (
    rhs.1, rhs.2, rhs.3, rhs.4, rhs.5
  )
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) > (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func > <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable,
  F: Comparable
>(
  lhs: (A, B, C, D, E, F),
  rhs: (A, B, C, D, E, F)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 > rhs.0 }
  /*tail*/ return (
    lhs.1, lhs.2, lhs.3, lhs.4, lhs.5
  ) > (
    rhs.1, rhs.2, rhs.3, rhs.4, rhs.5
  )
}

/// Returns a Boolean value indicating whether the first tuple is ordered
/// after or the same as the second in a lexicographical ordering.
///
/// Given two tuples `(a1, a2, ..., aN)` and `(b1, b2, ..., bN)`, the first
/// tuple is after or the same as the second tuple if and only if
/// `a1 > b1` or (`a1 == b1` and
/// `(a2, ..., aN) >= (b2, ..., bN)`).
///
/// - Parameters:
///   - lhs: A tuple of `Comparable` elements.
///   - rhs: Another tuple of elements of the same type as `lhs`.
@inlinable // trivial-implementation
public func >= <
  A: Comparable,
  B: Comparable,
  C: Comparable,
  D: Comparable,
  E: Comparable,
  F: Comparable
>(
  lhs: (A, B, C, D, E, F),
  rhs: (A, B, C, D, E, F)
) -> Bool {
  if lhs.0 != rhs.0 { return lhs.0 >= rhs.0 }
  /*tail*/ return (
    lhs.1, lhs.2, lhs.3, lhs.4, lhs.5
  ) >= (
    rhs.1, rhs.2, rhs.3, rhs.4, rhs.5
  )
}