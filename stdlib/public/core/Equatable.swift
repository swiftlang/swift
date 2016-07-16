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

//===----------------------------------------------------------------------===//
// Equatable
//===----------------------------------------------------------------------===//

/// A type that can be compared for value equality.
///
/// Types that conform to the `Equatable` protocol can be compared for equality
/// using the is-equal-to operator (`==`) or inequality using the
/// is-not-equal-to operator (`!=`). Most basic types in the Swift standard
/// library conform to `Equatable`.
///
/// Some sequence and collection operations can be used more simply when the
/// elements conform to `Equatable`. For example, to check whether an array
/// contains a particular value, you can pass the value itself to the
/// `contains(_:)` method when the array's element conforms to `Equatable`
/// instead of providing a closure that determines equivalence. The following
/// example shows how the `contains(_:)` method can be used with an array of
/// strings.
///
///     let students = ["Nora", "Fern", "Ryan", "Rainer"]
///
///     let nameToCheck = "Ryan"
///     if students.contains(nameToCheck) {
///         print("\(nameToCheck) is signed up!")
///     } else {
///         print("No record of \(nameToCheck).")
///     }
///     // Prints "Ryan is signed up!"
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
/// To adopt the `Equatable` protocol, implement the "is equal to" operator
/// (`==`). The standard library provides an implementation for the "is not
/// equal to" operator (`!=`) for any `Equatable` type, which calls the custom
/// `==` function and negates its result.
///
/// As an example, consider a `StreetAddress` structure that holds the parts of
/// a street address: a house or building number, the street name, and an
/// optional unit number. Here's the initial declaration of the
/// `StreetAddress` type:
///
///     struct StreetAddress {
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
///     extension StreetAddress: Equatable { }
///     func ==(lhs: StreetAddress, rhs: StreetAddress) -> Bool {
///         return
///             lhs.number == rhs.number &&
///             lhs.street == rhs.street &&
///             lhs.unit == rhs.unit
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
///     }
///
///     func ==(lhs: IntegerRef, rhs: IntegerRef) -> Bool {
///         return lhs.value == rhs.value
///     }
///
/// The implementation of the `==` function returns the same value whether
/// its two arguments are the same instance or are two different instances
/// with the same integer stored in their `value` properties. For example:
///
///     let a = IntegerRef(100)
///     let b = IntegerRef(100)
///
///     print(a == a, a == b, separator: ", ")
///     // Prints "true, true"
///
/// Class instance identity, on the other hand, is compared using the
/// triple-equals "is identical to" operator (`===`). For example:
///
///     let c = a
///     print(a === c, b === c, separator: ", ")
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

/// Returns a Boolean value indicating whether two values are not equal.
///
/// Inequality is the inverse of equality. For any values `a` and `b`, `a != b`
/// implies that `a == b` is `false`.
///
/// This is the default implementation of the is-not-equal-to operator (`!=`)
/// for any type that conforms to `Equatable`.
///
/// - Parameters:
///   - lhs: A value to compare.
///   - rhs: Another value to compare.
public func != <T : Equatable>(lhs: T, rhs: T) -> Bool {
  return !(lhs == rhs)
}

//===----------------------------------------------------------------------===//
// Reference comparison
//===----------------------------------------------------------------------===//

/// Returns `true` iff `lhs` and `rhs` are references to the same object
/// instance (in other words, are identical pointers).
///
/// - SeeAlso: `Equatable`, `==`
public func === (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
  switch (lhs, rhs) {
  case let (l?, r?):
    return Bool(Builtin.cmp_eq_RawPointer(
        Builtin.bridgeToRawPointer(Builtin.castToNativeObject(l)),
        Builtin.bridgeToRawPointer(Builtin.castToNativeObject(r))
      ))
  case (nil, nil):
    return true
  default:
    return false
  }
}

public func !== (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
  return !(lhs === rhs)
}


