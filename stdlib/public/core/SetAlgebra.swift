//===--- SetAlgebra.swift - Protocols for set operations ------------------===//
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
//
//  
//
//===----------------------------------------------------------------------===//

/// A generalized set whose distinct elements are not necessarily
/// disjoint.
///
/// In a model of `SetAlgebraType`, some elements may subsume other
/// elements, where
///
/// > `a` **subsumes** `b` iff `([a] as Self).isSupersetOf([b])`
///
/// In many models of `SetAlgebraType` such as `Set<T>`, `a`
/// *subsumes* `b` if and only if `a == b`, but that is not always the
/// case.  For example, option sets typically do not satisfy that
/// property.
///
/// Two elements are **disjoint** when neither one *subsumes* the other.
///
/// - SeeAlso: `OptionSetType`.
///
/// - Axioms, where `S` conforms to `SetAlgebraType`, `x` and `y` are
///   of type `S`, and `e` is of type `S.Element`:
///
///   - `S() == []`
///   - `x.intersect(x) == x`
///   - `x.intersect([]) == []`
///   - `x.union(x) == x`
///   - `x.union([]) == x`
///   - `x.contains(e)` implies `x.union(y).contains(e)`
///   - `x.union(y).contains(e)` implies `x.contains(e) || y.contains(e)`
///   - `x.contains(e) && y.contains(e)` iff `x.intersect(y).contains(e)`
///   - `x.isSubsetOf(y)` iff `y.isSupersetOf(x)`
///   - `x.isStrictSupersetOf(y)` iff `x.isSupersetOf(y) && x != y`
///   - `x.isStrictSubsetOf(y)` iff `x.isSubsetOf(y) && x != y`
public protocol SetAlgebraType : Equatable, ArrayLiteralConvertible {

  typealias DefaultImplementations = ()

  /// A type for which `Self` provides a containment test.
  typealias Element
  
  /// Creates an empty set.
  ///
  /// - Equivalent to `[] as Self`
  init()
  
  /// Returns `true` if `self` contains `member`.
  ///
  /// - Equivalent to `self.intersect([member]) == [member]`
  func contains(member: Element) -> Bool

  /// Returns the set of elements contained in `self`, in `other`, or in
  /// both `self` and `other`.
  func union(other: Self) -> Self
  
  /// Returns the set of elements contained in both `self` and `other`.
  func intersect(other: Self) -> Self

  /// Returns the set of elements contained in `self` or in `other`,
  /// but not in both `self` and `other`.
  func exclusiveOr(other: Self) -> Self

  /// If `member` is not already contained in `self`, inserts it.
  ///
  /// - Equivalent to `self.unionInPlace([member])`
  /// - Postcondition: `self.contains(member)`
  mutating func insert(member: Element)
  
  /// If `member` is contained in `self`, removes and returns it.
  /// Otherwise, removes all elements subsumed by `member` and returns
  /// `nil`.
  ///
  /// - Postcondition: `self.intersect([member]).isEmpty`
  mutating func remove(member: Element) -> Element?

  /// Insert all elements of `other` into `self`.
  ///
  /// - Equivalent to replacing `self` with `self.union(other)`.
  /// - Postcondition: `self.isSupersetOf(other)`
  mutating func unionInPlace(other: Self)

  /// Removes all elements of `self` that are not also present in
  /// `other`.
  ///
  /// - Equivalent to replacing `self` with `self.intersect(other)`
  /// - Postcondition: `self.isSubsetOf(other)`
  mutating func intersectInPlace(other: Self)

  /// Replaces `self` with a set containing all elements contained in
  /// either `self` or `other`, but not both.
  ///
  /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
  mutating func exclusiveOrInPlace(other: Self)  

  //===--- Requirements with default implementations ----------------------===//
  /// Return true iff `self.intersect(other).isEmpty`.
  func subtract(other: Self) -> Self
  /// Return true iff every element of `self` is contained in `other`.
  func isSubsetOf(other: Self) -> Bool
  /// Return true iff `self.intersect(other).isEmpty`.
  func isDisjointWith(other: Self) -> Bool
  /// Return true iff every element of `other` is contained in `self`.
  func isSupersetOf(other: Self) -> Bool
  /// Return true iff `self.contains(e)` is `false` for all `e`.
  var isEmpty: Bool { get }
  
  /// Creates the set containing all elements of `sequence`.
  init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)

  /// Removes all elements of `other` from `self`.
  ///
  /// - Equivalent to replacing `self` with `self.subtract(other)`.
  mutating func subtractInPlace(other: Self)

  /// Returns `true` iff `a` subsumes `b`.
  ///
  /// - Equivalent to `([a] as Self).isSupersetOf([b])`
  static func element(a: Element, subsumes b: Element) -> Bool

  /// Returns `true` iff `a` is disjoint with `b`.
  ///
  /// Two elements are disjoint when neither one subsumes the other.
  ///
  /// - SeeAlso: `Self.element(_, subsumes:_)`
  static func element(a: Element, isDisjointWith b: Element) -> Bool
  
}

/// `SetAlgebraType` requirements for which default implementations
/// are supplied.
///
/// - Note: A type conforming to `SetAlgebraType` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension SetAlgebraType where DefaultImplementations == () {
  /// Creates the set containing all elements of `sequence`.
  public init<
    S : SequenceType where S.Generator.Element == Element
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
  /// - Equivalent to replacing `self` with `self.subtract(other)`.
  final
  public mutating func subtractInPlace(other: Self) {
    self.intersectInPlace(self.exclusiveOr(other))
  }

  /// Returns true iff every element of `self` is contained in `other`.
  final
  public func isSubsetOf(other: Self) -> Bool {
    return self.intersect(other) == self
  }

  /// Returns true iff every element of `other` is contained in `self`.
  final
  public func isSupersetOf(other: Self) -> Bool {
    return other.isSubsetOf(self)
  }

  /// Returns true iff `self.intersect(other).isEmpty`.
  final
  public func isDisjointWith(other: Self) -> Bool {
    return self.intersect(other).isEmpty
  }

  /// Returns true iff `self.intersect(other).isEmpty`.
  final
  public func subtract(other: Self) -> Self {
    return self.intersect(self.exclusiveOr(other))
  }

  /// Returns true iff `self.contains(e)` is `false` for all `e`.
  final
  public var isEmpty: Bool {
    return self == Self()
  }

  /// Returns true iff every element of `other` is contained in `self`
  /// and `self` contains an element that is not contained in `other`.
  final
  public func isStrictSupersetOf(other: Self) -> Bool {
    return self.isSupersetOf(other) && self != other
  }

  /// Return true iff every element of `self` is contained in `other`
  /// and `other` contains an element that is not contained in `self`.
  final
  public func isStrictSubsetOf(other: Self) -> Bool {
    return other.isStrictSupersetOf(self)
  }

  /// Returns `true` iff `a` subsumes `b`.
  ///
  /// - Equivalent to `([a] as Self).isSupersetOf([b])`
  final
  public static func element(a: Element, subsumes b: Element) -> Bool {
    return ([a] as Self).isSupersetOf([b])
  }

  /// Returns `true` iff `a` is disjoint with `b`.
  ///
  /// Two elements are disjoint when neither one subsumes the other.
  ///
  /// - SeeAlso: `Self.element(_, subsumes:_)`
  final
  public static func element(a: Element, isDisjointWith b: Element) -> Bool {
    return ([a] as Self).isDisjointWith([b])
  }
}
