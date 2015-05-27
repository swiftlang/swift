//===--- OptionSet.swift --------------------------------------------------===//
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

/// Supplies convenient conformance to `SetAlgebraType` for any type
/// whose `RawValue` is a `BitwiseOperationsType`.  For example:
///
///     struct PackagingOptions : OptionSetType {
///       let rawValue: Int
///       init(rawValue: Int) { self.rawValue = rawValue }
///     
///       static let Box = PackagingOptions(rawValue: 1)
///       static let Carton = PackagingOptions(rawValue: 2)
///       static let Bag = PackagingOptions(rawValue: 4)
///       static let Satchel = PackagingOptions(rawValue: 8)
///       static let BoxOrBag: PackagingOptions = [Box, Bag]
///       static let BoxOrCartonOrBag: PackagingOptions = [Box, Carton, Bag]
///     }
///
/// In the example above, `PackagingOptions.Element` is the same type
/// as `PackagingOptions`, and instance `a` subsumes instance `b` if
/// and only if `a.rawValue & b.rawValue == b.rawValue`.
public protocol OptionSetType : SetAlgebraType, RawRepresentable {
  // We can't constrain the associated Element type to be the same as
  // Self, but we can do almost as well with a default and a
  // constrained extension
  
  /// An `OptionSet`'s `Element` type is normally `Self`.
  typealias Element = Self

  // FIXME: This initializer should just be the failable init from
  // RawRepresentable. Unfortunately, current language limitations
  // that prevent non-failable initializers from forwarding to
  // failable ones would prevent us from generating the non-failing
  // default (zero-argument) initializer.  Since OptionSetType's main
  // purpose is to create convenient conformances to SetAlgebraType,
  // we opt for a non-failable initializer.
  
  /// Convert from a value of `RawValue`, succeeding unconditionally.
  init(rawValue: RawValue)
}

/// `OptionSetType` requirements for which default implementations
/// are supplied.
///
/// - Note: A type conforming to `OptionSetType` can implement any of
///  these initializers or methods, and those implementations will be
///  used in lieu of these defaults.
extension OptionSetType {
  /// Returns the set of elements contained in `self`, in `other`, or in
  /// both `self` and `other`.
  final
  public func union(other: Self) -> Self {
    var r: Self = Self(rawValue: self.rawValue)
    r.unionInPlace(other)
    return r
  }
  
  /// Returns the set of elements contained in both `self` and `other`.
  final
  public func intersect(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.intersectInPlace(other)
    return r
  }
  
  /// Returns the set of elements contained in `self` or in `other`,
  /// but not in both `self` and `other`.
  final
  public func exclusiveOr(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.exclusiveOrInPlace(other)
    return r
  }
}

/// `OptionSetType` requirements for which default implementations are
/// supplied when `Element == Self`, which is the default.
///
/// - Note: A type conforming to `OptionSetType` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension OptionSetType where Element == Self {
  /// Returns `true` if `self` contains `member`.
  ///
  /// - Equivalent to `self.intersect([member]) == [member]`
  final
  public func contains(member: Self) -> Bool {
    return self.isSupersetOf(member)
  }
  
  /// If `member` is not already contained in `self`, insert it.
  ///
  /// - Equivalent to `self.unionInPlace([member])`
  /// - Postcondition: `self.contains(member)`
  final
  public mutating func insert(member: Element) {
    self.unionInPlace(member)
  }
  
  /// If `member` is contained in `self`, remove and return it.
  /// Otherwise, return `nil`.
  ///
  /// - Postcondition: `self.intersect([member]).isEmpty`
  final
  public mutating func remove(member: Element) -> Element? {
    let r = isSupersetOf(member) ? Optional(member) : nil
    self.subtractInPlace(member)
    return r
  }
}

/// `OptionSetType` requirements for which default implementations are
/// supplied when `RawValue` conforms to `BitwiseOperationsType`,
/// which is the usual case.  Each distinct bit of an option set's
/// `.rawValue` corresponds to a disjoint element of the option set.
///
/// - `union` is implemented as a bitwise "or" (`|`) of `rawValue`s
/// - `intersection` is implemented as a bitwise "and" (`|`) of `rawValue`s
/// - `exclusiveOr` is implemented as a bitwise "exclusive or" (`^`) of `rawValue`s
///
/// - Note: A type conforming to `OptionSetType` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension OptionSetType where RawValue : BitwiseOperationsType {
  /// Create an empty instance.
  ///
  /// - Equivalent to `[] as Self`
  public init() {
    self.init(rawValue: .allZeros)
  }

  /// Insert all elements of `other` into `self`.
  ///
  /// - Equivalent to replacing `self` with `self.union(other)`.
  /// - Postcondition: `self.isSupersetOf(other)`
  final
  public mutating func unionInPlace(other: Self) {
    self = Self(rawValue: self.rawValue | other.rawValue)
  }
  
  /// Remove all elements of `self` that are not also present in
  /// `other`.
  ///
  /// - Equivalent to replacing `self` with `self.intersect(other)`
  /// - Postcondition: `self.isSubsetOf(other)`
  final
  public mutating func intersectInPlace(other: Self) {
    self = Self(rawValue: self.rawValue & other.rawValue)
  }
  
  /// Replace `self` with a set containing all elements contained in
  /// either `self` or `other`, but not both.
  ///
  /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
  final
  public mutating func exclusiveOrInPlace(other: Self) {
    self = Self(rawValue: self.rawValue ^ other.rawValue)
  }
}

