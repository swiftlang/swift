//===--- SetAlgebra.swift - Protocols for set operations ------------------===//
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
//
//  
//
//===----------------------------------------------------------------------===//

/// A mutable set of a given `Element` type.
///
/// - Note: Unlike ordinary set types, the `Element` type of an
///   `OptionSet` is identical to the `OptionSet` type itself. This
///   protocol is specifically designed to accomodate both kinds of
///   set.
///
/// - SeeAlso: `Set`, `OptionSet`.
public protocol SetAlgebra : Equatable, ArrayLiteralConvertible {
  // FIXME: write tests for SetAlgebra
  
  /// A type for which `Self` provides a containment test.
  associatedtype Element
  
  /// Creates an empty set.
  ///
  /// - Equivalent to `[] as Self`
  init()
  
  /// Returns `true` if `self` contains `member`.
  ///
  /// - Equivalent to `self.intersection([member]) == [member]`
  @warn_unused_result
  func contains(member: Element) -> Bool

  /// Returns the set of elements contained in `self`, in `other`, or in
  /// both `self` and `other`.
  ///
  /// - Note: if `self` and `other` contain elements that are equal
  ///   but distinguishable (e.g. via `===`), which of these elements is 
  ///   present in the result is unspecified.
  @warn_unused_result
  func union(other: Self) -> Self
  
  /// Returns the set of elements contained in both `self` and `other`.
  ///
  /// - Note: if `self` and `other` contain elements that are equal
  ///   but distinguishable (e.g. via `===`), which of these elements is 
  ///   present in the result is unspecified.
  @warn_unused_result
  func intersection(other: Self) -> Self

  /// Returns the set of elements contained in `self` or in `other`,
  /// but not in both `self` and `other`.
  @warn_unused_result
  func symmetricDifference(other: Self) -> Self

  /// If `newMember` is not already contained in `self`, inserts it.
  ///
  /// - Returns: `(true, newMember)` if `e` was not contained in `self`.
  ///   Otherwise, returns `(false, oldMember)`, where `oldMember` is the
  ///   member of `self` equal to `newMember` (which may be distinguishable
  ///   from `newMember`, e.g. via `===`).
  ///
  /// - Postcondition: `self.contains(newMember)`.
  mutating func insert(
    newMember: Element
  ) -> (inserted: Bool, memberAfterInsert: Element)
  
  /// If `self` intersects `[e]`, removes and returns an element `r`
  /// such that `self.intersection([e]) == [r]`; returns `nil`
  /// otherwise.
  ///
  /// - Note: for ordinary sets where `Self` is not the same type as
  ///   `Element`, `s.remove(e)` removes and returns an element equal
  ///   to `e` (which may be distinguishable from `e`, e.g. via
  ///   `===`), or returns `nil` if no such element existed.
  ///
  /// - Postcondition: `self.intersection([e]).isEmpty`
  mutating func remove(e: Element) -> Element?

  /// Inserts `e` unconditionally.
  ///
  /// - Returns: a former member `r` of `self` such that
  ///   `self.intersection([e]) == [r]` if `self.intersection([e])` was
  ///   non-empty.  Returns `nil` otherwise.
  ///
  /// - Note: for ordinary sets where `Self` is not the same type as
  ///   `Element`, `s.update(with: e)` returns an element equal
  ///   to `e` (which may be distinguishable from `e`, e.g. via
  ///   `===`), or returns `nil` if no such element existed.
  ///
  /// - Postcondition: `self.contains(e)`
  mutating func update(with e: Element) -> Element?
  
  /// Insert all elements of `other` into `self`.
  ///
  /// - Equivalent to replacing `self` with `self.union(other)`.
  /// - Postcondition: `self.isSuperset(of: other)`
  mutating func formUnion(other: Self)

  /// Removes all elements of `self` that are not also present in
  /// `other`.
  ///
  /// - Equivalent to replacing `self` with `self.intersection(other)`
  /// - Postcondition: `self.isSubset(of: other)`
  mutating func formIntersection(other: Self)

  /// Replaces `self` with a set containing all elements contained in
  /// either `self` or `other`, but not both.
  ///
  /// - Equivalent to replacing `self` with `self.symmetricDifference(other)`
  mutating func formSymmetricDifference(other: Self)  

  //===--- Requirements with default implementations ----------------------===//
  /// Returns the set of elements contained in `self` but not in `other`.
  @warn_unused_result
  func subtracting(other: Self) -> Self

  /// Returns `true` iff every element of `self` is contained in `other`.
  @warn_unused_result
  func isSubset(of other: Self) -> Bool

  /// Returns `true` iff `self.intersection(other).isEmpty`.
  @warn_unused_result
  func isDisjoint(with other: Self) -> Bool

  /// Returns `true` iff every element of `other` is contained in `self`.
  @warn_unused_result
  func isSuperset(of other: Self) -> Bool

  /// Returns `true` iff `self.contains(e)` is `false` for all `e`.
  var isEmpty: Bool { get }
  
  /// Creates the set containing all elements of `sequence`.
  init<S : Sequence where S.Iterator.Element == Element>(_ sequence: S)

  /// Removes all elements of `other` from `self`.
  ///
  /// - Equivalent to replacing `self` with `self.subtracting(other)`.
  mutating func subtract(other: Self)
}

/// `SetAlgebra` requirements for which default implementations
/// are supplied.
///
/// - Note: A type conforming to `SetAlgebra` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension SetAlgebra {
  /// Creates the set containing all elements of `sequence`.
  public init<
    S : Sequence where S.Iterator.Element == Element
  >(_ sequence: S) {
    self.init()
    for e in sequence { insert(e) }
  }

  /// Creates a set containing all elements of the given `arrayLiteral`.
  ///
  /// This initializer allows an array literal containing
  /// `Self.Element` to represent an instance of the set, wherever it
  /// is implied by the type context.
  public init(arrayLiteral: Element...) {
    self.init(arrayLiteral)
  }

  /// Removes all elements of `other` from `self`.
  ///
  /// - Equivalent to replacing `self` with `self.subtracting(other)`.
  public mutating func subtract(other: Self) {
    self.formIntersection(self.symmetricDifference(other))
  }

  /// Returns `true` iff every element of `self` is contained in `other`.
  @warn_unused_result
  public func isSubset(of other: Self) -> Bool {
    return self.intersection(other) == self
  }

  /// Returns `true` iff every element of `other` is contained in `self`.
  @warn_unused_result
  public func isSuperset(of other: Self) -> Bool {
    return other.isSubset(of: self)
  }

  /// Returns `true` iff `self.intersection(other).isEmpty`.
  @warn_unused_result
  public func isDisjoint(with other: Self) -> Bool {
    return self.intersection(other).isEmpty
  }

  /// Returns the set of elements contained in `self` but not in `other`.
  @warn_unused_result
  public func subtracting(other: Self) -> Self {
    return self.intersection(self.symmetricDifference(other))
  }

  /// Returns `true` iff `self.contains(e)` is `false` for all `e`.
  public var isEmpty: Bool {
    return self == Self()
  }

  /// Returns `true` iff every element of `other` is contained in `self`
  /// and `self` contains an element that is not contained in `other`.
  @warn_unused_result
  public func isStrictSuperset(of other: Self) -> Bool {
    return self.isSuperset(of: other) && self != other
  }

  /// Returns `true` iff every element of `self` is contained in `other`
  /// and `other` contains an element that is not contained in `self`.
  @warn_unused_result
  public func isStrictSubset(of other: Self) -> Bool {
    return other.isStrictSuperset(of: self)
  }
}

@available(*, unavailable, renamed: "SetAlgebra")
public typealias SetAlgebraType = SetAlgebra

