//===--- OptionSet.swift - Test for library-only option sets --------------===//
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
// RUN: %target-run-simple-swift

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
protocol SetAlgebraDispatchType {
  typealias Element
  func subtract(other: Self) -> Self
  func isSubsetOf(other: Self) -> Bool
  func isDisjointWith(other: Self) -> Bool
  func isSupersetOf(other: Self) -> Bool
  var isEmpty: Bool { get }
  
  init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S)
  mutating func subtractInPlace(other: Self)

  static func element(a: Element, subsumes b: Element) -> Bool  
}

/// A generalized set whose distinct elements may not be disjoint.
///
/// In an `OptionSetType`, some elements may subsume other elements, where
///
/// > `a` **subsumes** `b` iff `[a].isSupersetOf([b] as Self)`
///
/// In many models of `SetAlgebraType` such as `Set<T>`, `a`
/// *subsumes* `b` if and only if `a == b`, but that is not always the
/// case.  For example, option sets typically do not satisfy that
/// property.
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
protocol SetAlgebraType : Equatable, ArrayLiteralConvertible,
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
/// are supplied.  A type conforming to `SetAlgebraType` can implement
/// any of these initializers or methods, and those implementations
/// will be used in lieu of these defaults.
extension SetAlgebraType {
  /// The set containing all elements of `sequence`.
  init<S : SequenceType where S.Generator.Element == Element>(_ sequence: S) {
    self.init()
    for e in sequence { insert(e) }
  }

  /// The set containing all elements of the given `arrayLiteral`.
  ///
  /// This initializer allows an array literal containing
  /// `Self.Element` to represent an instance of the set, wherever it
  /// is implied by the type context.
  init(arrayLiteral: Element...) {
    self.init(arrayLiteral)
  }

  /// Remove all elements of `other` from `self`.
  ///
  /// - Equivalent to replacing `self` with `self.subtract(other)`.
  final mutating func subtractInPlace(other: Self) {
    self.intersectInPlace(self.exclusiveOr(other))
  }

  /// Return true iff every element of `self` is contained in `other`.
  final func isSubsetOf(other: Self) -> Bool {
    return self.intersect(other) == self
  }

  /// Return true iff every element of `other` is contained in `self`.
  final func isSupersetOf(other: Self) -> Bool {
    return other.isSubsetOf(self)
  }

  /// Return true iff `self.intersect(other).isEmpty`.
  final func isDisjointWith(other: Self) -> Bool {
    return self.intersect(other).isEmpty
  }

  /// Return true iff `self.intersect(other).isEmpty`.
  final func subtract(other: Self) -> Self {
    return self.intersect(self.exclusiveOr(other))
  }

  /// Return true iff `self.contains(e)` is `false` for all `e`.
  final var isEmpty: Bool {
    return self == Self()
  }

  /// Return true iff every element of `other` is contained in `self`
  /// and `self` contains an element that is not contained in `other`.
  final func isStrictSupersetOf(other: Self) -> Bool {
    return self.isSupersetOf(other) && self != other
  }

  /// Return true iff every element of `self` is contained in `other`
  /// and `other` contains an element that is not contained in `self`.
  final func isStrictSubsetOf(other: Self) -> Bool {
    return other.isStrictSupersetOf(self)
  }

  /// Returns `true` iff `a` subsumes `b`.
  ///
  /// - Equivalent to `([a] as Self).isSupersetOf([b])`
  final
  static func element(a: Element, subsumes b: Element) -> Bool {
    return ([a] as Self).isSupersetOf([b])
  }
}

/// `Set` supports set algebra operations.
extension Set : SetAlgebraType {}

/// Returns `true` iff `lhs.rawValue == rhs.rawValue`.
func == <
  T : RawRepresentable where T.RawValue : Equatable
>(lhs: T, rhs: T) -> Bool {
  return lhs.rawValue == rhs.rawValue
}

/// Returns `true` iff `lhs.rawValue != rhs.rawValue`.
func != <
  T : RawRepresentable where T.RawValue : Equatable
>(lhs: T, rhs: T) -> Bool {
  return lhs.rawValue != rhs.rawValue
}

// This overload is needed for ambiguity resolution against the
// implementation of != for T : Equatable
/// Returns `true` iff `lhs.rawValue != rhs.rawValue`.
func != <
  T : Equatable where T : RawRepresentable, T.RawValue : Equatable
>(lhs: T, rhs: T) -> Bool {
  return lhs.rawValue != rhs.rawValue
}

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
protocol OptionSetType : SetAlgebraType, RawRepresentable {
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
/// A type conforming to `OptionSetType` can implement any of these
/// initializers or methods, and those implementations will be used in
/// lieu of these defaults.
extension OptionSetType {
  /// Returns the set of elements contained in `self`, in `other`, or in
  /// both `self` and `other`.
  final
  func union(other: Self) -> Self {
    var r: Self = Self(rawValue: self.rawValue)
    r.unionInPlace(other)
    return r
  }
  
  /// Returns the set of elements contained in both `self` and `other`.
  final
  func intersect(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.intersectInPlace(other)
    return r
  }
  
  /// Returns the set of elements contained in `self` or in `other`,
  /// but not in both `self` and `other`.
  final
  func exclusiveOr(other: Self) -> Self {
    var r = Self(rawValue: self.rawValue)
    r.exclusiveOrInPlace(other)
    return r
  }
}

/// `OptionSetType` requirements for which default implementations are
/// supplied when `Element == Self`, which is the default.
///
/// A type conforming to `OptionSetType` can implement any of these
/// initializers or methods, and those implementations will be used in
/// lieu of these defaults.
extension OptionSetType where Element == Self {
  /// Returns `true` if `self` contains `member`.
  ///
  /// - Equivalent to `self.intersect([member]) == [member]`
  final
  func contains(member: Self) -> Bool {
    return self.isSupersetOf(member)
  }
  
  /// If `member` is not already contained in `self`, insert it.
  ///
  /// - Equivalent to `self.unionInPlace([member])`
  /// - Postcondition: `self.contains(member)`
  final
  mutating func insert(member: Element) {
    self.unionInPlace(member)
  }
  
  /// If `member` is contained in `self`, remove and return it.
  /// Otherwise, return `nil`.
  ///
  /// - Postcondition: `self.intersect([member]).isEmpty`
  final
  mutating func remove(member: Element) -> Element? {
    let r = isSupersetOf(member) ? Optional(member) : nil
    self.subtractInPlace(member)
    return r
  }
}

/// `OptionSetType` requirements for which default implementations are
/// supplied when `RawValue` conforms to `BitwiseOperationsType`,
/// which is the usual case.
///
/// A type conforming to `OptionSetType` can implement any of these
/// initializers or methods, and those implementations will be used in
/// lieu of these defaults.
extension OptionSetType where RawValue : BitwiseOperationsType {
  /// Create an empty instance.
  ///
  /// - Equivalent to `[] as Self`
  init() {
    self.init(rawValue: .allZeros)
  }

  /// Insert all elements of `other` into `self`.
  ///
  /// - Equivalent to replacing `self` with `self.union(other)`.
  /// - Postcondition: `self.isSupersetOf(other)`
  final
  mutating func unionInPlace(other: Self) {
    self = Self(rawValue: self.rawValue | other.rawValue)
  }
  
  /// Remove all elements of `self` that are not also present in
  /// `other`.
  ///
  /// - Equivalent to replacing `self` with `self.intersect(other)`
  /// - Postcondition: `self.isSubsetOf(other)`
  final
  mutating func intersectInPlace(other: Self) {
    self = Self(rawValue: self.rawValue & other.rawValue)
  }
  
  /// Replace `self` with a set containing all elements contained in
  /// either `self` or `other`, but not both.
  ///
  /// - Equivalent to replacing `self` with `self.exclusiveOr(other)`
  final
  mutating func exclusiveOrInPlace(other: Self) {
    self = Self(rawValue: self.rawValue ^ other.rawValue)
  }
}

//===--- Tests ------------------------------------------------------------===//

struct PackagingOptions : OptionSetType {
  let rawValue: Int
  init(rawValue: Int) { self.rawValue = rawValue }

  static let Box = PackagingOptions(rawValue: 1)
  static let Carton = PackagingOptions(rawValue: 2)
  static let Bag = PackagingOptions(rawValue: 4)
  static let Satchel = PackagingOptions(rawValue: 8)
  static let BoxOrBag: PackagingOptions = [Box, Bag]
  static let BoxOrCartonOrBag: PackagingOptions = [Box, Carton, Bag]
  static let SatchelOrBag = Satchel.union(Bag)
}

debugPrint(PackagingOptions.Box)
debugPrint(PackagingOptions.Carton)
debugPrint(PackagingOptions.Bag)
debugPrint(PackagingOptions.Satchel)
debugPrint([.Satchel, .Bag, .Box] as PackagingOptions)
debugPrint(PackagingOptions.BoxOrCartonOrBag)
debugPrint(PackagingOptions.BoxOrBag != [.Box, .Satchel])
debugPrint([.Box, .Bag] == PackagingOptions.BoxOrBag)

/*
// Hack this part out if you want to play with OptionSet outside the
// test framework.
import StdlibUnittest

var tests = TestSuite("OptionSet")

tests.test("basics") {
  enum Options : Int { case one, two, three, four }

  var x = OptionSet<Options>()
  x = OptionSet(.one, .two)
  expectEqual(x, OptionSet(.one) | OptionSet(.two))

  let y = OptionSet<Options>(.four)

  let z = x | y
  expectEqual(OptionSet(.one, .two, .four), z)
  
  expectEqual(nil, ~z & z)
}

runAllTests()
*/
