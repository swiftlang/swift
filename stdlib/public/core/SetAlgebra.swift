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
/// You use types that conform to the `SetAlgebra` protocol when you need
/// efficient membership tests or mathematical set operations such as
/// intersection, union, and substraction. In the standard library, you can
/// use the `Set` type with elements of any hashable type, or you can easily
/// create bit masks with `SetAlgebra` conformance using the `OptionSet`
/// protocol. See those types for more information.
///
/// - Note: Unlike ordinary set types, the `Element` type of an
///   `OptionSet` is identical to the `OptionSet` type itself. This
///   protocol is specifically designed to accomodate both kinds of
///   set.
///
/// Conforming to the SetAlgebra Protocol
/// =====================================
///
/// When implementing a custom type that conforms to the `SetAlgebra` protocol,
/// you must implement the required initializers and methods. For the
/// inherited methods to work properly, conforming types must meet the
/// following axioms. Assume that `S` is a custom type that conforms to the
/// `SetAlgebra` protocol, `x` and `y` are instances of `S`, and `e` is of
/// type `S.Element`---the type that the set holds.
///
/// - `S() == []`
/// - `x.intersect(x) == x`
/// - `x.intersect([]) == []`
/// - `x.union(x) == x`
/// - `x.union([]) == x`
/// - `x.contains(e)` implies `x.union(y).contains(e)`
/// - `x.union(y).contains(e)` implies `x.contains(e) || y.contains(e)`
/// - `x.contains(e) && y.contains(e)` if and only if
///   `x.intersect(y).contains(e)`
/// - `x.isSubsetOf(y)` if and only if `y.isSupersetOf(x)`
/// - `x.isStrictSupersetOf(y)` if and only if `x.isSupersetOf(y) && x != y`
/// - `x.isStrictSubsetOf(y)` if and only if `x.isSubsetOf(y) && x != y`
///
/// Nondisjoint Elements
/// --------------------
///
/// The `SetAlgebra` protocol supports set types for which elements of the set
/// are not necessarily unique. In a `SetAlgebra` model, some elements may
/// *subsume* other elements. Here, an element `e` subsumes an element `f` if
/// and only if the modeled set of `e` is a superset of the modeled set of
/// `f`. Two elements are *disjoint* when neither one subsumes the other.
///
/// In many concrete implementations of `SetAlgebra`, such as `Set<Element>`,
/// an element `e` subsumes an element `f` if and only if the two elements are
/// equal, but that is not always the case. For example, an element of an
/// option set may subsume another element without the two being equal.
///
/// Consider an option set called `Directions` that defines the four directions
/// you can move in a game. The `Directions` type declares the set of each of
/// those four distinct directions as static properties, and it declares a
/// fifth set of all directions as another static property.
///
///     struct Directions: OptionSet {
///         let rawValue: UInt8
///
///         static let up    = Directions(rawValue: 1 << 0)
///         static let down  = Directions(rawValue: 1 << 1)
///         static let left  = Directions(rawValue: 1 << 2)
///         static let right = Directions(rawValue: 1 << 3)
///
///         static let allDirections: Directions = [up, down, left, right]
///     }
///
/// In this example, the `allDirections` property is both an instance of *and*
/// an element of the `Directions` option set. As such, it can be said to
/// subsume each of the distinct directions, whereas the distinct directions
/// are disjoint from one another.
///
///     print(Directions.element(.allDirections, subsumes: .up))
///     // Prints "true"
///     print(Directions.element(.right, isDisjointWith: .left))
///     // Prints "true"
///
/// - SeeAlso: `OptionSet`, `Set`
public protocol SetAlgebra : Equatable, ArrayLiteralConvertible {
  // FIXME: write tests for SetAlgebra
  
  /// A type for which `Self` provides a containment test.
  associatedtype Element
  
  /// Creates an empty set.
  ///
  /// This initializer is equivalent to initializing with an empty array
  /// literal. For example, you create an empty `Set` instance with either
  /// this initializer or with an empty array literal.
  ///
  ///     var emptySet = Set<Int>()
  ///     print(emptySet.isEmpty)
  ///     // Prints "true"
  ///
  ///     emptySet = []
  ///     print(emptySet.isEmpty)
  ///     // Prints "true"
  init()
  
  /// Returns a Boolean value that indicates whether the given element exists
  /// in the set.
  /// 
  /// For example:
  ///
  ///     let primes: Set = [2, 3, 5, 7]
  ///     let x = 5
  ///     if primes.contains(x) {
  ///         print("\(x) is prime!")
  ///     } else {
  ///         print("\(x). Not prime.")
  ///     }
  ///     // Prints "5 is prime!"
  ///
  /// - Parameter member: An element to look for in the set.
  /// - Returns: `true` if `member` exists in the set; otherwise, `false`.
  @warn_unused_result
  func contains(_ member: Element) -> Bool

  /// Returns the set of elements contained in `self`, in `other`, or in
  /// both `self` and `other`.
  ///
  /// - Note: if `self` and `other` contain elements that are equal
  ///   but distinguishable (e.g. via `===`), which of these elements is 
  ///   present in the result is unspecified.
  @warn_unused_result
  func union(_ other: Self) -> Self
  
  /// Returns a new set with the elements that are common to both this set and
  /// the given set.
  ///
  /// For example:
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors: Set = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     let bothNeighborsAndEmployees = employees.intersect(neighbors)
  ///     print(bothNeighborsAndEmployees)
  ///     // Prints "["Bethany", "Eric"]"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: A new set.
  /// - Note: if `self` and `other` contain elements that are equal
  ///   but distinguishable (e.g. via `===`), which of these elements is 
  ///   present in the result is unspecified.
  @warn_unused_result
  func intersection(_ other: Self) -> Self

  /// Returns a new set with the elements that are either in this set or in the
  /// given set, but not in both.
  ///
  /// For example:
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Diana", "Eric"]
  ///     let neighbors: Set = ["Bethany", "Eric", "Forlani"]
  ///     let eitherNeighborsOrEmployees = employees.exclusiveOr(neighbors)
  ///     print(eitherNeighborsOrEmployees)
  ///     // Prints "["Diana", "Forlani", "Alicia"]"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: A new set.
  @warn_unused_result
  func symmetricDifference(_ other: Self) -> Self

  /// If `newMember` is not already contained in `self`, inserts it.
  ///
  /// If the element is already contained in the set, this method has no
  /// effect. In this example, a new element is inserted into `classDays`, a
  /// set of days of the week. When an existing element is inserted, the
  /// `classDays` set does not change.
  ///
  ///     enum DayOfTheWeek: Int {
  ///         case Sunday, Monday, Tuesday, Wednesday, Thursday,
  ///             Friday, Saturday
  ///     }
  ///
  ///     var classDays: Set<DayOfTheWeek> = [.Wednesday, .Friday]
  ///     classDays.insert(.Monday)
  ///     print(classDays)
  ///     // Prints "[.Friday, .Wednesday, .Monday]"
  ///
  ///     classDays.insert(.Friday)
  ///     print(classDays)
  ///     // Prints "[.Friday, .Wednesday, .Monday]"
  ///
  /// - Returns: `(true, newMember)` if `e` was not contained in `self`.
  ///   Otherwise, returns `(false, oldMember)`, where `oldMember` is the
  ///   member of `self` equal to `newMember` (which may be distinguishable
  ///   from `newMember`, e.g. via `===`).
  ///
  /// - Postcondition: `self.contains(newMember)`.
  mutating func insert(
    _ newMember: Element
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
  mutating func remove(_ e: Element) -> Element?

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
  mutating func formUnion(_ other: Self)

  /// Removes all elements of `self` that are not also present in
  /// `other`.
  ///
  /// - Equivalent to replacing `self` with `self.intersection(other)`
  /// - Postcondition: `self.isSubset(of: other)`
  mutating func formIntersection(_ other: Self)

  /// Replaces `self` with a set containing all elements contained in
  /// either `self` or `other`, but not both.
  ///
  /// - Equivalent to replacing `self` with `self.symmetricDifference(other)`
  mutating func formSymmetricDifference(_ other: Self)  

  //===--- Requirements with default implementations ----------------------===//
  /// Returns the set of elements contained in `self` but not in `other`.
  @warn_unused_result
  func subtracting(_ other: Self) -> Self

  /// Returns a Boolean value that indicates whether the set is a subset of
  /// another set.
  ///
  /// Set *A* is a subset of another set *B* if every member of *A* is also a
  /// member of *B*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(attendees.isSubsetOf(employees))
  ///     // Prints "true"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: `true` if the set is a subset of `other`; otherwise, `false`.
  @warn_unused_result
  func isSubset(of other: Self) -> Bool

  /// Returns `true` iff `self.intersection(other).isEmpty`.
  @warn_unused_result
  func isDisjoint(with other: Self) -> Bool

  /// Returns a Boolean value that indicates whether the set is a superset of
  /// the given set.
  ///
  /// Set *A* is a superset of another set *B* if every member of *B* is also a
  /// member of *A*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(employees.isSupersetOf(attendees))
  ///     // Prints "true"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: `true` if the set is a superset of `possibleSubset`;
  ///   otherwise, `false`.
  @warn_unused_result
  func isSuperset(of other: Self) -> Bool

  /// A Boolean value that indicates whether the set has no elements.
  var isEmpty: Bool { get }
  
  /// Creates a new set from a finite sequence of items.
  ///
  /// Use this initializer to create a new set from an existing sequence, like
  /// an array or a range:
  ///
  ///     let validIndices = Set(0..<7).subtract([2, 4, 5])
  ///     print(validIndices)
  ///     // Prints "[6, 0, 1, 3]"
  ///
  /// - Parameter sequence: The elements to use as members of the new set.
  init<S : Sequence where S.Iterator.Element == Element>(_ sequence: S)

  /// Removes the elements of the given set from this set.
  ///
  /// For example:
  ///
  /// - Equivalent to replacing `self` with `self.subtracting(other)`.
  mutating func subtract(_ other: Self)
}

/// `SetAlgebra` requirements for which default implementations
/// are supplied.
///
/// - Note: A type conforming to `SetAlgebra` can implement any of
///   these initializers or methods, and those implementations will be
///   used in lieu of these defaults.
extension SetAlgebra {
  /// Creates a new set from a finite sequence of items.
  ///
  /// Use this initializer to create a new set from an existing sequence, like
  /// an array or a range:
  ///
  ///     let validIndices = Set(0..<7).subtract([2, 4, 5])
  ///     print(validIndices)
  ///     // Prints "[6, 0, 1, 3]"
  ///
  /// - Parameter sequence: The elements to use as members of the new set.
  public init<
    S : Sequence where S.Iterator.Element == Element
  >(_ sequence: S) {
    self.init()
    for e in sequence { insert(e) }
  }

  /// Creates a set containing the elements of the given array literal.
  ///
  /// Don't directly call this initializer, which is used by the compiler when
  /// you use an array literal. Instead, create a new set using an array
  /// literal as its value by enclosing a comma-separated list of values in
  /// square brackets. You can use an array literal anywhere a set is expected
  /// by the type context.
  ///
  /// Here, an set of strings is created from an array literal holding only
  /// strings:
  ///
  ///     let ingredients: Set = ["cocoa beans", "sugar", "cocoa butter", "salt"]
  ///     if ingredients.isSupersetOf(["sugar", "salt"]) {
  ///         print("Whatever it is, it's bound to be delicious!")
  ///     }
  ///     // Prints "Whatever it is, it's bound to be delicious!"
  ///
  /// - Parameter arrayLiteral: A variadic list of elements of the new set.
  public init(arrayLiteral: Element...) {
    self.init(arrayLiteral)
  }

  /// Removes the elements of the given set from this set.
  ///
  /// For example:
  ///
  /// - Equivalent to replacing `self` with `self.subtracting(other)`.
  public mutating func subtract(_ other: Self) {
    self.formIntersection(self.symmetricDifference(other))
  }

  /// Returns a Boolean value that indicates whether the set is a subset of
  /// another set.
  ///
  /// Set *A* is a subset of another set *B* if every member of *A* is also a
  /// member of *B*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(attendees.isSubsetOf(employees))
  ///     // Prints "true"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: `true` if the set is a subset of `other`; otherwise, `false`.
  @warn_unused_result
  public func isSubset(of other: Self) -> Bool {
    return self.intersection(other) == self
  }

  /// Returns a Boolean value that indicates whether the set is a superset of
  /// the given set.
  ///
  /// Set *A* is a superset of another set *B* if every member of *B* is also a
  /// member of *A*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(employees.isSupersetOf(attendees))
  ///     // Prints "true"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: `true` if the set is a superset of `possibleSubset`;
  ///   otherwise, `false`.
  @warn_unused_result
  public func isSuperset(of other: Self) -> Bool {
    return other.isSubset(of: self)
  }

  /// Returns `true` iff `self.intersection(other).isEmpty`.
  @warn_unused_result
  public func isDisjoint(with other: Self) -> Bool {
    return self.intersection(other).isEmpty
  }

  /// Returns a new set containing the elements of this set that do not occur
  /// in the given set.
  ///
  /// For example:
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let neighbors: Set = ["Bethany", "Eric", "Forlani", "Greta"]
  ///     let nonNeighbors = employees.subtract(neighbors)
  ///     print(nonNeighbors)
  ///     // Prints "["Diana", "Chris", "Alicia"]"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: A new set.
  @warn_unused_result
  public func subtracting(_ other: Self) -> Self {
    return self.intersection(self.symmetricDifference(other))
  }

  /// A Boolean value that indicates whether the set has no elements.
  public var isEmpty: Bool {
    return self == Self()
  }

  /// Returns a Boolean value that indicates whether this set is a strict
  /// superset of the given set.
  ///
  /// Set *A* is a strict superset of another set *B* if every member of *B* is
  /// also a member of *A* and *A* contains at least one element that is *not*
  /// a member of *B*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(employees.isStrictSupersetOf(attendees))
  ///     // Prints "true"
  ///     print(employees.isStrictSupersetOf(employees))
  ///     // Prints "false"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: `true` if the set is a strict superset of `other`; otherwise,
  ///   `false`.
  @warn_unused_result
  public func isStrictSuperset(of other: Self) -> Bool {
    return self.isSuperset(of: other) && self != other
  }

  /// Returns a Boolean value that indicates whether this set is a strict
  /// subset of the given set.
  ///
  /// Set *A* is a strict subset of another set *B* if every member of *A* is
  /// also a member of *B* and *B* contains at least one element that is not a
  /// member of *A*.
  ///
  ///     let employees: Set = ["Alicia", "Bethany", "Chris", "Diana", "Eric"]
  ///     let attendees: Set = ["Alicia", "Bethany", "Diana"]
  ///     print(attendees.isStrictSubsetOf(employees))
  ///     // Prints "true"
  ///
  ///     // A set is never a strict subset of itself:
  ///     print(attendees.isStrictSubsetOf(attendees))
  ///     // Prints "false"
  ///
  /// - Parameter other: A set of the same type as the current set.
  /// - Returns: `true` if the set is a strict subset of `other`; otherwise,
  ///   `false`.
  @warn_unused_result
  public func isStrictSubset(of other: Self) -> Bool {
    return other.isStrictSuperset(of: self)
  }
}

@available(*, unavailable, renamed: "SetAlgebra")
public typealias SetAlgebraType = SetAlgebra

