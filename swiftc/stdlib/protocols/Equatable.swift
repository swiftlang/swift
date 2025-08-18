//===----------------------------------------------------------------------===//
//
// This source file is part of the swiftc open source project
//
// Licensed under Apache License v2.0 with Runtime Library Exception
//
//===----------------------------------------------------------------------===//

/// A type that can be compared for value equality.
///
/// Types that conform to the `Equatable` protocol can be compared for equality
/// using the equal-to operator (`==`) or inequality using the not-equal-to
/// operator (`!=`). Most basic types in the Swift standard library conform to
/// `Equatable`.
///
/// Some sequence and collection operations can use equality to find or exclude
/// elements. For example, the `contains(_:)` method uses the equal-to operator
/// (`==`) to find a matching element in a collection.
///
///     let students = ["Kofi", "Abena", "Peter", "Kweku", "Akosua"]
///     let nameToCheck = "Peter"
///     if students.contains(nameToCheck) {
///         print("\(nameToCheck) is signed up!")
///     } else {
///         print("No record of \(nameToCheck).")
///     }
///     // Prints "Peter is signed up!"
///
/// ## Conforming to the Equatable Protocol
///
/// Adding `Equatable` conformance to your custom types means that you can use
/// the equal-to operator (`==`) and not-equal-to operator (`!=`) when comparing
/// instances. `Equatable` is also the base protocol for the `Hashable` and
/// `Comparable` protocols, which allow more uses of your custom type.
///
/// The Swift compiler can automatically provide an implementation of the
/// `Equatable` requirements for you under certain circumstances:
///
/// - For a `struct`, all its stored properties must conform to `Equatable`.
/// - For an `enum`, all its associated values must conform to `Equatable`.
///   (An `enum` without associated values has `Equatable` conformance even
///   without the declaration.)
///
/// To customize your type's `Equatable` conformance, to adopt `Equatable` in a
/// type that doesn't meet the criteria listed above, or to extend an existing
/// type to conform to `Equatable`, implement the equal-to operator (`==`) as
/// a static method of your type. The standard library provides an
/// implementation for the not-equal-to operator (`!=`) for any `Equatable`
/// type, which calls the custom `==` function and negates the result.
///
/// As an example, consider a `StreetAddress` class that holds the parts of a
/// street address: a house number, street name, and unit number. Here's the
/// initial declaration of the `StreetAddress` type:
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
/// Now suppose you have an array of addresses that you want to check for a
/// particular address. To use the `contains(_:)` method, the `StreetAddress`
/// type must conform to the `Equatable` protocol. The simplest way to add
/// `Equatable` conformance is to declare conformance in the original
/// declaration and implement the `==` operator as a static method.
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
/// compare instances or call the `Equatable`-based methods, such as
/// `contains(_:)`, on collections of `StreetAddress` instances.
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

// MARK: - Default implementations

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