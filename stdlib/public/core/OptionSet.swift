//===--- OptionSet.swift --------------------------------------------------===//
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

/// Supplies convenient conformance to `SetAlgebra` for any type
/// whose `RawValue` is a `BitwiseOperations`.  For example:
///
///     struct PackagingOptions : OptionSet {
///       let rawValue: Int
///       init(rawValue: Int) { self.rawValue = rawValue }
///     
///       static let box = PackagingOptions(rawValue: 1)
///       static let carton = PackagingOptions(rawValue: 2)
///       static let bag = PackagingOptions(rawValue: 4)
///       static let satchel = PackagingOptions(rawValue: 8)
///       static let boxOrBag: PackagingOptions = [box, bag]
///       static let boxOrCartonOrBag: PackagingOptions = [box, carton, bag]
///     }
///
/// In the example above, `PackagingOptions.Element` is the same type
/// as `PackagingOptions`, and `Elements` whose `rawValue`s have bits
/// in common have a non-empty intersection when treated as sets.  For
/// example,
///
///     PackagingOptions.boxOrBag.intersection(.bag).rawValue // 4
public protocol OptionSet : SetAlgebra, RawRepresentable {
  // We can't constrain the associated Element type to be the same as
  // Self, but we can do almost as well with a default and a
  // constrained extension
  
  /// An `OptionSet`'s `Element` type is normally `Self`.
  associatedtype Element = Self

  // FIXME: This initializer should just be the failable init from
  // RawRepresentable. Unfortunately, current language limitations
  // that prevent non-failable initializers from forwarding to
  // failable ones would prevent us from generating the non-failing
  // default (zero-argument) initializer.  Since OptionSet's main
  // purpose is to create convenient conformances to SetAlgebra,
  // we opt for a non-failable initializer.
  
  /// Convert from a value of `RawValue`, succeeding unconditionally.
  init(rawValue: RawValue)
}

/// `OptionSet` requirements for which default implementations
/// are supplied.
///
/// - Note: A type conforming to `OptionSet` can implement any of
///  these initializers or methods, and those implementations will be
///  used in lieu of these defaults.
extension OptionSet {
  /// Returns the set of elements contained in `self`, in `other`, or in
  /// both `self` and `other`.
  @warn_unused_result
  public func union(other: Self) -> Self {
    var r: Self = Self(rawValue: self.rawValue)
    r.formUnion(other)
    return r
  }
  
  /// Returns the set of elements contained in both `self` and `other`.
  @warn_unused_result
  public func intersection(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.formIntersection(other)
    return r
  }
  
  /// Returns the set of elements contained in `self` or in `other`,
  /// but not in both `self` and `other`.
  @warn_unused_result
  public func symmetricDifference(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.formSymmetricDifference(other)
    return r
  }
}

/// `OptionSet` requirements for which default implementations are
/// supplied when `Element == Self`, which is the default.
///
/// - Note: A type conforming to `OptionSet` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension OptionSet where Element == Self {
  /// Returns `true` if `self` contains `member`.
  ///
  /// - Equivalent to `self.intersection([member]) == [member]`
  @warn_unused_result
  public func contains(member: Self) -> Bool {
    return self.isSuperset(of: member)
  }
  
  /// If `newMember` is not already contained in `self`, inserts it.
  ///
  /// - Returns: `(true, newMember)` if `e` was not contained in `self`.
  ///   Otherwise, returns `(false, oldMember)`, where `oldMember` is the
  ///   member of `self` equal to `newMember`.
  ///
  /// - Postcondition: `self.contains(newMember)`.
  public mutating func insert(
    newMember: Element
  ) -> (inserted: Bool, memberAfterInsert: Element) {
    let oldMember = self.intersection(newMember)
    let shouldInsert = oldMember != newMember
    let result = (
      inserted: shouldInsert,
      memberAfterInsert: shouldInsert ? newMember : oldMember)
    if shouldInsert {
      self.formUnion(newMember)
    }
    return result
  }
  
  /// If `member` is contained in `self`, remove and return it.
  /// Otherwise, return `nil`.
  ///
  /// - Postcondition: `self.intersection([member]).isEmpty`
  public mutating func remove(member: Element) -> Element? {
    let r = isSuperset(of: member) ? Optional(member) : nil
    self.subtract(member)
    return r
  }

  /// Inserts `e` unconditionally.
  ///
  /// - Returns: a former member `r` of `self` such that
  ///   `self.intersection([e]) == [r]` if `self.intersection([e])` was
  ///   non-empty.  Returns `nil` otherwise.
  ///
  /// - Postcondition: `self.contains(e)`
  public mutating func update(with e: Element) -> Element? {
    let r = self.intersection(e)
    self.formUnion(e)
    return r.isEmpty ? nil : r
  }
}

/// `OptionSet` requirements for which default implementations are
/// supplied when `RawValue` conforms to `BitwiseOperations`,
/// which is the usual case.  Each distinct bit of an option set's
/// `.rawValue` corresponds to a disjoint value of the `OptionSet`.
///
/// - `union` is implemented as a bitwise "or" (`|`) of `rawValue`s
/// - `intersection` is implemented as a bitwise "and" (`&`) of
///   `rawValue`s
/// - `symmetricDifference` is implemented as a bitwise "exclusive or"
///    (`^`) of `rawValue`s
///
/// - Note: A type conforming to `OptionSet` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension OptionSet where RawValue : BitwiseOperations {
  /// Create an empty instance.
  ///
  /// - Equivalent to `[] as Self`
  public init() {
    self.init(rawValue: .allZeros)
  }

  /// Insert all elements of `other` into `self`.
  ///
  /// - Equivalent to replacing `self` with `self.union(other)`.
  /// - Postcondition: `self.isSuperset(of: other)`
  public mutating func formUnion(other: Self) {
    self = Self(rawValue: self.rawValue | other.rawValue)
  }
  
  /// Remove all elements of `self` that are not also present in
  /// `other`.
  ///
  /// - Equivalent to replacing `self` with `self.intersection(other)`
  /// - Postcondition: `self.isSubset(of: other)`
  public mutating func formIntersection(other: Self) {
    self = Self(rawValue: self.rawValue & other.rawValue)
  }
  
  /// Replace `self` with a set containing all elements contained in
  /// either `self` or `other`, but not both.
  ///
  /// - Equivalent to replacing `self` with `self.symmetricDifference(other)`
  public mutating func formSymmetricDifference(other: Self) {
    self = Self(rawValue: self.rawValue ^ other.rawValue)
  }
}

@available(*, unavailable, renamed: "OptionSet")
public typealias OptionSetType = OptionSet

