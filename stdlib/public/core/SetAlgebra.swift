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
public protocol SetAlgebraType : Equatable, ArrayLiteralConvertible,
    SetAlgebraDispatchType {

  /// A type for which `Self` provides a containment test.
  typealias Element
  
  /// Create an empty instance.
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

  /// If `member` is not already contained in `self`, insert it.
  ///
  /// - Equivalent to `self.unionInPlace([member])`
  /// - Postcondition: `self.contains(member)`
  mutating func insert(member: Element)
  
  /// If `member` is contained in `self`, remove and return it.
  /// Otherwise, remove all elements subsumed by `member` and return
  /// `nil`.
  ///
  /// - Postcondition: `self.intersect([member]).isEmpty`
  mutating func remove(member: Element) -> Element?

  /// Insert all elements of `other` into `self`.
  ///
  /// - Equivalent to replacing `self` with `self.union(other)`.
  /// - Postcondition: `self.isSupersetOf(other)`
  mutating func unionInPlace(other: Self)

  /// Remove all elements of `self` that are not also present in
  /// `other`.
  ///
  /// - Equivalent to replacing `self` with `self.intersect(other)`
  /// - Postcondition: `self.isSubsetOf(other)`
  mutating func intersectInPlace(other: Self)

  /// Replace `self` with a set containing all elements contained in
  /// either `self` or `other`, but not both.
  ///
  /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
  mutating func exclusiveOrInPlace(other: Self)  
}

/// `SetAlgebraType` requirements for which default implementations
/// are supplied.
///
/// - Note: A type conforming to `SetAlgebraType` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension SetAlgebraType {
  /// The set containing all elements of `sequence`.
  public init<
    S : SequenceType where S.Generator.Element == Element
  >(_ sequence: S) {
    self.init()
    for e in sequence { insert(e) }
  }

  /// The set containing all elements of the given `arrayLiteral`.
  ///
  /// This initializer allows an array literal containing
  /// `Self.Element` to represent an instance of the set, wherever it
  /// is implied by the type context.
  public init(arrayLiteral: Element...) {
    self.init(arrayLiteral)
  }

  /// Remove all elements of `other` from `self`.
  ///
  /// - Equivalent to replacing `self` with `self.subtract(other)`.
  final
  public mutating func subtractInPlace(other: Self) {
    self.intersectInPlace(self.exclusiveOr(other))
  }

  /// Return true iff every element of `self` is contained in `other`.
  final
  public func isSubsetOf(other: Self) -> Bool {
    return self.intersect(other) == self
  }

  /// Return true iff every element of `other` is contained in `self`.
  final
  public func isSupersetOf(other: Self) -> Bool {
    return other.isSubsetOf(self)
  }

  /// Return true iff `self.intersect(other).isEmpty`.
  final
  public func isDisjointWith(other: Self) -> Bool {
    return self.intersect(other).isEmpty
  }

  /// Return true iff `self.intersect(other).isEmpty`.
  final
  public func subtract(other: Self) -> Self {
    return self.intersect(self.exclusiveOr(other))
  }

  /// Return true iff `self.contains(e)` is `false` for all `e`.
  final
  public var isEmpty: Bool {
    return self == Self()
  }

  /// Return true iff every element of `other` is contained in `self`
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

/// Dispatching protocol for `SetAlgebraType`.
///
/// For all intents and purposes, you can and should ignore this
/// protocol.  All of its requirements are restated in the derived
/// `SetAlgebraType` protocol, either as requirements, or as
/// implementations in its protocol extension.
/// `SetAlgebraDispatchType` exists merely so that default generic
/// algorithms on `SetAlgebraType` will dispatch to more specific
/// operations given by a type that conforms to `SetAlgebraType`:
///
///     /// exercise two SetAlgebraType operations
///     func differenceIsEmpty<S: SetAlgebraType>(s1: S, s2: S) -> Bool {
///       return s1.subtract(s2).isEmpty
///     }
///
///     /// A type that supports set algebra
///     struct MySet : SetAlgebraType {
///       var isEmpty: Bool { /* specialized isEmpty implementation */ }
///       ...                 /* no subtract implementation given */
///     }
///
///     // calls the **specialized implementation of isEmpty** given above,
///     // and the **default implementation of subtract** given in the
///     // SetAlgebraType protocol extension.
///     differenceIsEmpty(MySet(), MySet()) 
///
/// - SeeAlso: `SetAlgebraType` for details of the APIs required here.
public protocol SetAlgebraDispatchType {
  typealias Element
  func subtract(other: Self) -> Self
  func isSubsetOf(other: Self) -> Bool
  func isDisjointWith(other: Self) -> Bool
  func isSupersetOf(other: Self) -> Bool
  var isEmpty: Bool { get }
  
  init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)
  mutating func subtractInPlace(other: Self)

  static func element(a: Element, subsumes b: Element) -> Bool  
  static func element(a: Element, isDisjointWith b: Element) -> Bool
}
