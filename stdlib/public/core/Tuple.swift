//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// Variadic functions for comparison operations on tuple types.

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
@_alwaysEmitIntoClient
public func ==<each B: Equatable>(lhs: (repeat each B), rhs: (repeat each B)) -> Bool {
  for (lhs, rhs) in repeat (each lhs, each rhs) {
    if lhs != rhs {
      return false
    }
  }
  return true
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
@_alwaysEmitIntoClient
public func !=<each B: Equatable>(lhs: (repeat each B), rhs: (repeat each B)) -> Bool {
  for (lhs, rhs) in repeat (each lhs, each rhs) {
    if lhs == rhs {
      return false
    }
  }
  return true
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
@_alwaysEmitIntoClient
public func < <each B: Comparable>(lhs: (repeat each B), rhs: (repeat each B)) -> Bool {
  for (lhs, rhs) in repeat (each lhs, each rhs) {
    if lhs >= rhs {
      return false
    }
  }
  return true
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
@_alwaysEmitIntoClient
public func <= <each B: Comparable>(lhs: (repeat each B), rhs: (repeat each B)) -> Bool {
  for (lhs, rhs) in repeat (each lhs, each rhs) {
    if lhs > rhs {
      return false
    }
  }
  return true
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
@_alwaysEmitIntoClient
public func > <each B: Comparable>(lhs: (repeat each B), rhs: (repeat each B)) -> Bool {
  for (lhs, rhs) in repeat (each lhs, each rhs) {
    if lhs <= rhs {
      return false
    }
  }
  return true
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
@_alwaysEmitIntoClient
public func >= <each B: Comparable>(lhs: (repeat each B), rhs: (repeat each B)) -> Bool {
  for (lhs, rhs) in repeat (each lhs, each rhs) {
    if lhs < rhs {
      return false
    }
  }
  return true
}

// MARK: - Old vestigial ABI symbols for old variadic tuple methods.

@usableFromInline
@_disfavoredOverload
internal func == (lhs: (), rhs: ()) -> Bool {
  return true
}

@usableFromInline
@_disfavoredOverload
internal func != (lhs: (), rhs: ()) -> Bool {
    return false
}

@usableFromInline
@_disfavoredOverload
internal func < (lhs: (), rhs: ()) -> Bool {
    return false
}

@usableFromInline
@_disfavoredOverload
internal func <= (lhs: (), rhs: ()) -> Bool {
    return true
}

@usableFromInline
@_disfavoredOverload
internal func > (lhs: (), rhs: ()) -> Bool {
    return false
}

@usableFromInline
@_disfavoredOverload
internal func >=(lhs: (), rhs: ()) -> Bool {
    return true
}

@usableFromInline
@_disfavoredOverload
internal func ==  <A: Equatable, B: Equatable>(lhs: (A,B), rhs: (A,B)) -> Bool {
  lhs == rhs
}

@usableFromInline
@_disfavoredOverload
internal func !=  <A: Equatable, B: Equatable>(lhs: (A,B), rhs: (A,B)) -> Bool {
  lhs != rhs
}

@usableFromInline
@_disfavoredOverload
internal func <  <A: Comparable, B: Comparable>(lhs: (A,B), rhs: (A,B)) -> Bool {
  lhs < rhs
}

@usableFromInline
@_disfavoredOverload
internal func <=  <A: Comparable, B: Comparable>(lhs: (A,B), rhs: (A,B)) -> Bool {
  lhs <= rhs
}

@usableFromInline
@_disfavoredOverload
internal func >  <A: Comparable, B: Comparable>(lhs: (A,B), rhs: (A,B)) -> Bool {
  lhs > rhs
}

@usableFromInline
@_disfavoredOverload
internal func >= <A: Comparable, B: Comparable>(lhs: (A,B), rhs: (A,B)) -> Bool {
  lhs >= rhs
}

@usableFromInline
@_disfavoredOverload
internal func ==  <A: Equatable, B: Equatable, C: Equatable>(lhs: (A,B,C), rhs: (A,B,C)) -> Bool {
  lhs == rhs
}

@usableFromInline
@_disfavoredOverload
internal func !=  <A: Equatable, B: Equatable, C: Equatable>(lhs: (A,B,C), rhs: (A,B,C)) -> Bool {
  lhs != rhs
}

@usableFromInline
@_disfavoredOverload
internal func <  <A: Comparable, B: Comparable, C: Comparable>(lhs: (A,B,C), rhs: (A,B,C)) -> Bool {
  lhs < rhs
}

@usableFromInline
@_disfavoredOverload
internal func <= <A: Comparable, B: Comparable, C: Comparable>(lhs: (A,B,C), rhs: (A,B,C)) -> Bool {
  lhs <= rhs
}

@usableFromInline
@_disfavoredOverload
internal func > <A: Comparable, B: Comparable, C: Comparable>(lhs: (A,B,C), rhs: (A,B,C)) -> Bool {
  lhs > rhs
}

@usableFromInline
@_disfavoredOverload
internal func >= <A: Comparable, B: Comparable, C: Comparable>(lhs: (A,B,C), rhs: (A,B,C)) -> Bool {
  lhs >= rhs
}

@usableFromInline
@_disfavoredOverload
internal func == <A: Equatable, B: Equatable, C: Equatable, D: Equatable>(lhs: (A,B,C,D), rhs: (A,B,C,D)) -> Bool {
  lhs == rhs
}

@usableFromInline
@_disfavoredOverload
internal func !=  <A: Equatable, B: Equatable, C: Equatable, D: Equatable>(lhs: (A,B,C,D), rhs: (A,B,C,D)) -> Bool {
  lhs != rhs
}

@usableFromInline
@_disfavoredOverload
internal func <  <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(lhs: (A,B,C,D), rhs: (A,B,C,D)) -> Bool {
  lhs < rhs
}

@usableFromInline
@_disfavoredOverload
internal func <=  <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(lhs: (A,B,C,D), rhs: (A,B,C,D)) -> Bool {
  lhs <= rhs
}

@usableFromInline
@_disfavoredOverload
internal func >  <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(lhs: (A,B,C,D), rhs: (A,B,C,D)) -> Bool {
  lhs > rhs
}

@usableFromInline
@_disfavoredOverload
internal func >= <A: Comparable, B: Comparable, C: Comparable, D: Comparable>(lhs: (A,B,C,D), rhs: (A,B,C,D)) -> Bool {
  lhs >= rhs
}

@usableFromInline
@_disfavoredOverload
internal func ==  <A: Equatable, B: Equatable, C: Equatable, D: Equatable, E: Equatable>(lhs: (A,B,C,D,E), rhs: (A,B,C,D,E)) -> Bool {
  lhs == rhs
}

@usableFromInline
@_disfavoredOverload
internal func !=  <A: Equatable, B: Equatable, C: Equatable, D: Equatable, E: Equatable>(lhs: (A,B,C,D,E), rhs: (A,B,C,D,E)) -> Bool {
  lhs != rhs
}

@usableFromInline
@_disfavoredOverload
internal func <  <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable>(lhs: (A,B,C,D,E), rhs: (A,B,C,D,E)) -> Bool {
  lhs < rhs
}

@usableFromInline
@_disfavoredOverload
internal func <=  <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable>(lhs: (A,B,C,D,E), rhs: (A,B,C,D,E)) -> Bool {
  lhs <= rhs
}

@usableFromInline
@_disfavoredOverload
internal func >  <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable>(lhs: (A,B,C,D,E), rhs: (A,B,C,D,E)) -> Bool {
  lhs > rhs
}

@usableFromInline
@_disfavoredOverload
internal func >= <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable>(lhs: (A,B,C,D,E), rhs: (A,B,C,D,E)) -> Bool {
  lhs >= rhs
}

@usableFromInline
@_disfavoredOverload
internal func ==  <A: Equatable, B: Equatable, C: Equatable, D: Equatable, E: Equatable, F: Equatable>(lhs: (A,B,C,D,E,F), rhs: (A,B,C,D,E,F)) -> Bool {
  lhs == rhs
}

@usableFromInline
@_disfavoredOverload
internal func !=  <A: Equatable, B: Equatable, C: Equatable, D: Equatable, E: Equatable, F: Equatable>(lhs: (A,B,C,D,E,F), rhs: (A,B,C,D,E,F)) -> Bool {
  lhs != rhs
}

@usableFromInline
@_disfavoredOverload
internal func <  <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable, F: Comparable>(lhs: (A,B,C,D,E,F), rhs: (A,B,C,D,E,F)) -> Bool {
  lhs < rhs
}

@usableFromInline
@_disfavoredOverload
internal func <=  <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable, F: Comparable>(lhs: (A,B,C,D,E,F), rhs: (A,B,C,D,E,F)) -> Bool {
  lhs <= rhs
}

@usableFromInline
@_disfavoredOverload
internal func >  <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable, F: Comparable>(lhs: (A,B,C,D,E,F), rhs: (A,B,C,D,E,F)) -> Bool {
  lhs > rhs
}

@usableFromInline
@_disfavoredOverload
internal func >= <A: Comparable, B: Comparable, C: Comparable, D: Comparable, E: Comparable, F: Comparable>(lhs: (A,B,C,D,E,F), rhs: (A,B,C,D,E,F)) -> Bool {
  lhs >= rhs
}
