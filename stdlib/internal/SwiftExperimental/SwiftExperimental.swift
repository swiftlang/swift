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
// Experimental APIs of the Swift Standard Library
//
// This library contains experimental APIs that can be subject to change or
// removal.  We don't guarantee API or ABI stability for this library.
//
//===----------------------------------------------------------------------===//
import Swift

/// The function composition operator is the only user-defined operator that
/// operates on functions.  That's why the numeric value of precedence does
/// not matter right now.
infix operator ∘ {
  // The character is U+2218 RING OPERATOR.
  //
  // Confusables:
  //
  // U+00B0 DEGREE SIGN
  // U+02DA RING ABOVE
  // U+25CB WHITE CIRCLE
  // U+25E6 WHITE BULLET
  associativity left
  precedence 100
}

/// Compose functions.
///
///     (g ∘ f)(x) == g(f(x))
///
/// - Returns: a function that applies ``g`` to the result of applying ``f``
///   to the argument of the new function.
public func ∘<T, U, V>(g: (U) -> V, f: (T) -> U) -> ((T) -> V) {
  return { g(f($0)) }
}

infix operator ∖ { associativity left precedence 140 }
infix operator ∖= { associativity right precedence 90 assignment }
infix operator ∪ { associativity left precedence 140 }
infix operator ∪= { associativity right precedence 90 assignment }
infix operator ∩ { associativity left precedence 150 }
infix operator ∩= { associativity right precedence 90 assignment }
infix operator ⨁ { associativity left precedence 140 }
infix operator ⨁= { associativity right precedence 90 assignment }
infix operator ∈ { associativity left precedence 130 }
infix operator ∉ { associativity left precedence 130 }
infix operator ⊂ { associativity left precedence 130 }
infix operator ⊄ { associativity left precedence 130 }
infix operator ⊆ { associativity left precedence 130 }
infix operator ⊈ { associativity left precedence 130 }
infix operator ⊃ { associativity left precedence 130 }
infix operator ⊅ { associativity left precedence 130 }
infix operator ⊇ { associativity left precedence 130 }
infix operator ⊉ { associativity left precedence 130 }

/// - Returns: The relative complement of `lhs` with respect to `rhs`.
public func ∖ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Set<T> {
  return lhs.subtracting(rhs)
}

/// Assigns the relative complement between `lhs` and `rhs` to `lhs`.
public func ∖= <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: inout Set<T>, rhs: S) {
  lhs.subtract(rhs)
}

/// - Returns: The union of `lhs` and `rhs`.
public func ∪ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Set<T> {
  return lhs.union(rhs)
}

/// Assigns the union of `lhs` and `rhs` to `lhs`.
public func ∪= <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: inout Set<T>, rhs: S) {
  lhs.formUnion(rhs)
}

/// - Returns: The intersection of `lhs` and `rhs`.
public func ∩ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Set<T> {
  return lhs.intersection(rhs)
}

/// Assigns the intersection of `lhs` and `rhs` to `lhs`.
public func ∩= <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: inout Set<T>, rhs: S) {
  lhs.formIntersection(rhs)
}

/// - Returns: A set with elements in `lhs` or `rhs` but not in both.
public func ⨁ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Set<T> {
  return lhs.symmetricDifference(rhs)
}

/// Assigns to `lhs` the set with elements in `lhs` or `rhs` but not in both.
public func ⨁= <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: inout Set<T>, rhs: S) {
  lhs.formSymmetricDifference(rhs)
}

/// - Returns: True if `x` is in the set.
public func ∈ <T>(x: T, rhs: Set<T>) -> Bool {
  return rhs.contains(x)
}

/// - Returns: True if `x` is not in the set.
public func ∉ <T>(x: T, rhs: Set<T>) -> Bool {
  return !rhs.contains(x)
}

/// - Returns: True if `lhs` is a strict subset of `rhs`.
public func ⊂ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return lhs.isStrictSubset(of: rhs)
}

/// - Returns: True if `lhs` is not a strict subset of `rhs`.
public func ⊄ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return !lhs.isStrictSubset(of: rhs)
}

/// - Returns: True if `lhs` is a subset of `rhs`.
public func ⊆ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return lhs.isSubset(of: rhs)
}

/// - Returns: True if `lhs` is not a subset of `rhs`.
public func ⊈ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return !lhs.isSubset(of: rhs)
}

/// - Returns: True if `lhs` is a strict superset of `rhs`.
public func ⊃ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return lhs.isStrictSuperset(of: rhs)
}

/// - Returns: True if `lhs` is not a strict superset of `rhs`.
public func ⊅ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return !lhs.isStrictSuperset(of: rhs)
}

/// - Returns: True if `lhs` is a superset of `rhs`.
public func ⊇ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return lhs.isSuperset(of: rhs)
}

/// - Returns: True if `lhs` is not a superset of `rhs`.
public func ⊉ <
  T, S: Sequence where S.Iterator.Element == T
  >(lhs: Set<T>, rhs: S) -> Bool {
  return !lhs.isSuperset(of: rhs)
}
