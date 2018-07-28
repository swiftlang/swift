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

//===----------------------------------------------------------------------===//
// Equatable
//===----------------------------------------------------------------------===//

/// A type that can be compared for value equality.
///
/// Types that conform to the `Equatable` protocol can be compared for equality
/// using the equal-to operator (`==`) or inequality using the not-equal-to
/// operator (`!=`). Most basic types in the Swift standard library conform to
/// `Equatable`.
///
/// Some sequence and collection operations can be used more simply when the
/// elements conform to `Equatable`. For example, to check whether an array
/// contains a particular value, you can pass the value itself to the
/// `contains(_:)` method when the array's element conforms to `Equatable`
/// instead of providing a closure that determines equivalence. The following
/// example shows how the `contains(_:)` method can be used with an array of
/// strings.
///
///     let students = ["Kofi", "Abena", "Efua", "Kweku", "Akosua"]
///
///     let nameToCheck = "Kofi"
///     if students.contains(nameToCheck) {
///         print("\(nameToCheck) is signed up!")
///     } else {
///         print("No record of \(nameToCheck).")
///     }
///     // Prints "Kofi is signed up!"
///
/// Conforming to the Equatable Protocol
/// ====================================
///
/// Adding `Equatable` conformance to your custom types means that you can use
/// more convenient APIs when searching for particular instances in a
/// collection. `Equatable` is also the base protocol for the `Hashable` and
/// `Comparable` protocols, which allow more uses of your custom type, such as
/// constructing sets or sorting the elements of a collection.
///
/// You can rely on automatic synthesis of the `Equatable` protocol's
/// requirements for a custom type when you declare `Equatable` conformance in
/// the type's original declaration and your type meets these criteria:
///
/// - For a `struct`, all its stored properties must conform to `Equatable`.
/// - For an `enum`, all its associated values must conform to `Equatable`. (An
///   `enum` without associated values has `Equatable` conformance even
///   without the declaration.)
///
/// To customize your type's `Equatable` conformance, to adopt `Equatable` in a
/// type that doesn't meet the criteria listed above, or to extend an existing
/// type to conform to `Equatable`, implement the equal-to operator (`==`) as
/// a static method of your type. The standard library provides an
/// implementation for the not-equal-to operator (`!=`) for any `Equatable`
/// type, which calls the custom `==` function and negates its result.
///
/// As an example, consider a `StreetAddress` class that holds the parts of a
/// street address: a house or building number, the street name, and an
/// optional unit number. Here's the initial declaration of the
/// `StreetAddress` type:
///
///     class StreetAddress {
///         let number: String
///         let street: String
///         let unit: String?
///
///         init(_ number: String, _ street: String, unit: String? = nil) {
///             self.number = number
///             self.street = street
///             self.unit = unit
///         }
///     }
///
/// Now suppose you have an array of addresses that you need to check for a
/// particular address. To use the `contains(_:)` method without including a
/// closure in each call, extend the `StreetAddress` type to conform to
/// `Equatable`.
///
///     extension StreetAddress: Equatable {
///         static func == (lhs: StreetAddress, rhs: StreetAddress) -> Bool {
///             return
///                 lhs.number == rhs.number &&
///                 lhs.street == rhs.street &&
///                 lhs.unit == rhs.unit
///         }
///     }
///
/// The `StreetAddress` type now conforms to `Equatable`. You can use `==` to
/// check for equality between any two instances or call the
/// `Equatable`-constrained `contains(_:)` method.
///
///     let addresses = [StreetAddress("1490", "Grove Street"),
///                      StreetAddress("2119", "Maple Avenue"),
///                      StreetAddress("1400", "16th Street")]
///     let home = StreetAddress("1400", "16th Street")
///
///     print(addresses[0] == home)
///     // Prints "false"
///     print(addresses.contains(home))
///     // Prints "true"
///
/// Equality implies substitutability---any two instances that compare equally
/// can be used interchangeably in any code that depends on their values. To
/// maintain substitutability, the `==` operator should take into account all
/// visible aspects of an `Equatable` type. Exposing nonvalue aspects of
/// `Equatable` types other than class identity is discouraged, and any that
/// *are* exposed should be explicitly pointed out in documentation.
///
/// Since equality between instances of `Equatable` types is an equivalence
/// relation, any of your custom types that conform to `Equatable` must
/// satisfy three conditions, for any values `a`, `b`, and `c`:
///
/// - `a == a` is always `true` (Reflexivity)
/// - `a == b` implies `b == a` (Symmetry)
/// - `a == b` and `b == c` implies `a == c` (Transitivity)
///
/// Moreover, inequality is the inverse of equality, so any custom
/// implementation of the `!=` operator must guarantee that `a != b` implies
/// `!(a == b)`. The default implementation of the `!=` operator function
/// satisfies this requirement.
///
/// Equality is Separate From Identity
/// ----------------------------------
///
/// The identity of a class instance is not part of an instance's value.
/// Consider a class called `IntegerRef` that wraps an integer value. Here's
/// the definition for `IntegerRef` and the `==` function that makes it
/// conform to `Equatable`:
///
///     class IntegerRef: Equatable {
///         let value: Int
///         init(_ value: Int) {
///             self.value = value
///         }
///
///         static func == (lhs: IntegerRef, rhs: IntegerRef) -> Bool {
///             return lhs.value == rhs.value
///         }
///     }
///
/// The implementation of the `==` function returns the same value whether its
/// two arguments are the same instance or are two different instances with
/// the same integer stored in their `value` properties. For example:
///
///     let a = IntegerRef(100)
///     let b = IntegerRef(100)
///
///     print(a == a, a == b, separator: ", ")
///     // Prints "true, true"
///
/// Class instance identity, on the other hand, is compared using the
/// triple-equals identical-to operator (`===`). For example:
///
///     let c = a
///     print(c === a, c === b, separator: ", ")
///     // Prints "true, false"
public protocol Equatable {
  /// Returns a Boolean value indicating whether two values are equal.
  ///
  /// Equality is the inverse of inequality. For any values `a` and `b`,
  /// `a == b` implies that `a != b` is `false`.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  static func == (lhs: Self, rhs: Self) -> Bool
}

extension Equatable {
  /// Returns a Boolean value indicating whether two values are not equal.
  ///
  /// Inequality is the inverse of equality. For any values `a` and `b`, `a != b`
  /// implies that `a == b` is `false`.
  ///
  /// This is the default implementation of the not-equal-to operator (`!=`)
  /// for any type that conforms to `Equatable`.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  @_transparent
  public static func != (lhs: Self, rhs: Self) -> Bool {
    return !(lhs == rhs)
  }
}

//===----------------------------------------------------------------------===//
// Reference comparison
//===----------------------------------------------------------------------===//

/// Returns a Boolean value indicating whether two references point to the same
/// object instance.
///
/// This operator tests whether two instances have the same identity, not the
/// same value. For value equality, see the equal-to operator (`==`) and the
/// `Equatable` protocol.
///
/// The following example defines an `IntegerRef` type, an integer type with
/// reference semantics.
///
///     class IntegerRef: Equatable {
///         let value: Int
///         init(_ value: Int) {
///             self.value = value
///         }
///     }
///
///     func ==(lhs: IntegerRef, rhs: IntegerRef) -> Bool {
///         return lhs.value == rhs.value
///     }
///
/// Because `IntegerRef` is a class, its instances can be compared using the
/// identical-to operator (`===`). In addition, because `IntegerRef` conforms
/// to the `Equatable` protocol, instances can also be compared using the
/// equal-to operator (`==`).
///
///     let a = IntegerRef(10)
///     let b = a
///     print(a == b)
///     // Prints "true"
///     print(a === b)
///     // Prints "true"
///
/// The identical-to operator (`===`) returns `false` when comparing two
/// references to different object instances, even if the two instances have
/// the same value.
///
///     let c = IntegerRef(10)
///     print(a == c)
///     // Prints "true"
///     print(a === c)
///     // Prints "false"
///
/// - Parameters:
///   - lhs: A reference to compare.
///   - rhs: Another reference to compare.
@inlinable // FIXME(sil-serialize-all)
public func === (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return ObjectIdentifier(l) == ObjectIdentifier(r)
  case (nil, nil):
    return true
  default:
    return false
  }
}

/// Returns a Boolean value indicating whether two references point to
/// different object instances.
///
/// This operator tests whether two instances have different identities, not
/// different values. For value inequality, see the not-equal-to operator
/// (`!=`) and the `Equatable` protocol.
///
/// - Parameters:
///   - lhs: A reference to compare.
///   - rhs: Another reference to compare.
@inlinable // FIXME(sil-serialize-all)
public func !== (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
  return !(lhs === rhs)
}


