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
// Experimental APIs of the Swift Standard Library
//
// This library contains experimental APIs that can be subject to change or
// removal.  We don't guarantee API or ABI stability for this library.
//
//===----------------------------------------------------------------------===//
import Swift

/// The function composition operator is the only user-defined operator that
/// operates on functions.  That's why the exact precedence does not matter
/// right now.
infix operator ∘ : CompositionPrecedence
// The character is U+2218 RING OPERATOR.
//
// Confusables:
//
// U+00B0 DEGREE SIGN
// U+02DA RING ABOVE
// U+25CB WHITE CIRCLE
// U+25E6 WHITE BULLET

precedencegroup CompositionPrecedence {
  associativity: left
  higherThan: TernaryPrecedence
}

/// Compose functions.
///
///     (g ∘ f)(x) == g(f(x))
///
/// - Returns: a function that applies ``g`` to the result of applying ``f``
///   to the argument of the new function.
public func ∘<T, U, V>(g: @escaping (U) -> V, f: @escaping (T) -> U) -> ((T) -> V) {
  return { g(f($0)) }
}

infix operator ∖ : AdditionPrecedence
infix operator ∖= : AssignmentPrecedence
infix operator ∪ : AdditionPrecedence
infix operator ∪=  : AssignmentPrecedence
infix operator ∩ : MultiplicationPrecedence
infix operator ∩=  : AssignmentPrecedence
infix operator ⨁ : AdditionPrecedence
infix operator ⨁=  : AssignmentPrecedence
infix operator ∈ : ComparisonPrecedence
infix operator ∉ : ComparisonPrecedence
infix operator ⊂ : ComparisonPrecedence
infix operator ⊄ : ComparisonPrecedence
infix operator ⊆ : ComparisonPrecedence
infix operator ⊈ : ComparisonPrecedence
infix operator ⊃ : ComparisonPrecedence
infix operator ⊅ : ComparisonPrecedence
infix operator ⊇ : ComparisonPrecedence
infix operator ⊉ : ComparisonPrecedence

/// - Returns: The relative complement of `lhs` with respect to `rhs`.
public func ∖ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Set<T>
  where S.Iterator.Element == T {
  return lhs.subtracting(rhs)
}

/// Assigns the relative complement between `lhs` and `rhs` to `lhs`.
public func ∖= <T, S: Sequence>(lhs: inout Set<T>, rhs: S)
  where S.Iterator.Element == T {
  lhs.subtract(rhs)
}

/// - Returns: The union of `lhs` and `rhs`.
public func ∪ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Set<T>
  where S.Iterator.Element == T {
  return lhs.union(rhs)
}

/// Assigns the union of `lhs` and `rhs` to `lhs`.
public func ∪= <T, S: Sequence>(lhs: inout Set<T>, rhs: S)
  where S.Iterator.Element == T {
  lhs.formUnion(rhs)
}

/// - Returns: The intersection of `lhs` and `rhs`.
public func ∩ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Set<T>
  where S.Iterator.Element == T {
  return lhs.intersection(rhs)
}

/// Assigns the intersection of `lhs` and `rhs` to `lhs`.
public func ∩= <T, S: Sequence>(lhs: inout Set<T>, rhs: S)
  where S.Iterator.Element == T {
  lhs.formIntersection(rhs)
}

/// - Returns: A set with elements in `lhs` or `rhs` but not in both.
public func ⨁ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Set<T>
  where S.Iterator.Element == T {
  return lhs.symmetricDifference(rhs)
}

/// Assigns to `lhs` the set with elements in `lhs` or `rhs` but not in both.
public func ⨁= <T, S: Sequence>(lhs: inout Set<T>, rhs: S)
  where S.Iterator.Element == T {
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
public func ⊂ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return lhs.isStrictSubset(of: rhs)
}

/// - Returns: True if `lhs` is not a strict subset of `rhs`.
public func ⊄ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return !lhs.isStrictSubset(of: rhs)
}

/// - Returns: True if `lhs` is a subset of `rhs`.
public func ⊆ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return lhs.isSubset(of: rhs)
}

/// - Returns: True if `lhs` is not a subset of `rhs`.
public func ⊈ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return !lhs.isSubset(of: rhs)
}

/// - Returns: True if `lhs` is a strict superset of `rhs`.
public func ⊃ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return lhs.isStrictSuperset(of: rhs)
}

/// - Returns: True if `lhs` is not a strict superset of `rhs`.
public func ⊅ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return !lhs.isStrictSuperset(of: rhs)
}

/// - Returns: True if `lhs` is a superset of `rhs`.
public func ⊇ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return lhs.isSuperset(of: rhs)
}

/// - Returns: True if `lhs` is not a superset of `rhs`.
public func ⊉ <T, S: Sequence>(lhs: Set<T>, rhs: S) -> Bool
  where S.Iterator.Element == T {
  return !lhs.isSuperset(of: rhs)
}
