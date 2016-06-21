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
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Standardized aliases
//===----------------------------------------------------------------------===//
/// The return type of functions that don't explicitly specify a return type;
/// an empty tuple (i.e., `()`).
///
/// When declaring a function or method, you don't need to specify a return
/// type if no value will be returned. However, the type of a function,
/// method, or closure always includes a return type, which is `Void` if
/// otherwise unspecified.
///
/// Use `Void` or an empty tuple as the return type when declaring a
/// closure, function, or method that doesn't return a value.
///
///     // No return type declared:
///     func logMessage(s: String) {
///         print("Message: \(s)")
///     }
///
///     let logger: String -> Void = logMessage
///     logger("This is a void function")
///     // Prints "Message: This is a void function"
public typealias Void = ()

//===----------------------------------------------------------------------===//
// Aliases for floating point types
//===----------------------------------------------------------------------===//
// FIXME: it should be the other way round, Float = Float32, Double = Float64,
// but the type checker loses sugar currently, and ends up displaying 'FloatXX'
// in diagnostics.
/// A 32-bit floating point type.
public typealias Float32 = Float
/// A 64-bit floating point type.
public typealias Float64 = Double

//===----------------------------------------------------------------------===//
// Default types for unconstrained literals
//===----------------------------------------------------------------------===//
/// The default type for an otherwise-unconstrained integer literal.
public typealias IntegerLiteralType = Int
/// The default type for an otherwise-unconstrained floating point literal.
public typealias FloatLiteralType = Double

/// The default type for an otherwise-unconstrained Boolean literal.
///
/// When you create a constant or variable using one of the Boolean literals
/// `true` or `false`, the resulting type is determined by the
/// `BooleanLiteralType` alias. For example:
///
///     let isBool = true
///     print("isBool is a '\(isBool.dynamicType)'")
///     // Prints "isBool is a 'Bool'"
///
/// The type aliased by `BooleanLiteralType` must conform to the
/// `BooleanLiteralConvertible` protocol.
public typealias BooleanLiteralType = Bool

/// The default type for an otherwise-unconstrained unicode scalar literal.
public typealias UnicodeScalarType = String
/// The default type for an otherwise-unconstrained Unicode extended
/// grapheme cluster literal.
public typealias ExtendedGraphemeClusterType = String
/// The default type for an otherwise-unconstrained string literal.
public typealias StringLiteralType = String

//===----------------------------------------------------------------------===//
// Default types for unconstrained number literals
//===----------------------------------------------------------------------===//
// Integer literals are limited to 2048 bits.
// The intent is to have arbitrary-precision literals, but implementing that
// requires more work.
//
// Rationale: 1024 bits are enough to represent the absolute value of min/max
// IEEE Binary64, and we need 1 bit to represent the sign.  Instead of using
// 1025, we use the next round number -- 2048.
public typealias _MaxBuiltinIntegerType = Builtin.Int2048
#if !os(Windows) && (arch(i386) || arch(x86_64))
public typealias _MaxBuiltinFloatType = Builtin.FPIEEE80
#else
public typealias _MaxBuiltinFloatType = Builtin.FPIEEE64
#endif

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

/// The protocol to which all types implicitly conform.
///
/// The `Any` protocol can be used as the concrete type for an instance of any
/// type in Swift: a class, struct, or enumeration; a metatype, such as
/// `Int.self`; a tuple with any types of components; or a closure or function
/// type.
///
/// Casting Any Instances to a Known Type
/// =====================================
///
/// When you use `Any` as a concrete type, you must cast your instance back to
/// a known type before you can access its properties or methods. Instances
/// with a concrete type of `Any` maintain their original dynamic type and can
/// be cast to that type using one of the type-cast operators (`as`, `as?`, or
/// `as!`). 
/// 
/// For example, use `as?` to conditionally downcast the first object
/// in a heterogeneous array to a `String`.
///
///     let mixed: [Any] = ["one", "two", 3, true, {(x: Int) -> Int in x * 2 }]
///
///     let first = = numberObjects.firstObject
///     if let first = mixed.first as? String {
///         print("The first item, '\(first)', is a String")
///     }
///     // Prints("The first item, 'one', is a String")
///
/// If you have prior knowledge that an `Any` instance is an instance of
/// a particular type, you can use the `as!` operator to unconditionally
/// downcast. Performing an invalid cast results in a runtime error.
///
///     let second = mixed[1] as! String
///     print("'\(second)' is also a String")
///     // Prints "'two' is also a String"
///
/// In a `switch` statement, a value is cast to a type only when pattern
/// matching with that type succeeds. For that reason, you use the `as`
/// operator instead of the conditional `as?` or unconditional `as!`
/// operators.
///
///     for item in mixed {
///         switch item {
///         case let s as String:
///             print("String: \(s)")
///         case let i as Int:
///             print("Integer: \(i)")
///         case let b as Bool:
///             print("Bool: \(b)")
///         case let f as Int -> Int:
///             print("Function: 2 * 5 = \(f(5))")
///         default:
///             print("Unrecognized type")
///         }
///     }
///     // Prints "String: one"
///     // Prints "String: two"
///     // Prints "Integer: 3"
///     // Prints "Bool: true"
///     // Prints "Function: 2 * 5 = 10"
///     
/// - SeeAlso: `AnyObject`, `AnyClass`
public typealias Any = protocol<>

#if _runtime(_ObjC)
/// The protocol to which all classes implicitly conform.
///
/// You use `AnyObject` when you need the flexibility of an untyped object or
/// when you use bridged Objective-C methods and properties that return an
/// untyped result. `AnyObject` can be used as the concrete type for an
/// instance of any class, class type, or class-only protocol. For example:
///
///     class FloatRef {
///         let value: Float
///         init(_ value: Float) {
///             self.value = value
///         }
///     }
///
///     let x = FloatRef(2.3)
///     let y: AnyObject = x
///     let z: AnyObject = FloatRef.self
///
/// `AnyObject` can also be used as the concrete type for an instance of a type
/// that bridges to an Objective-C class. Many value types in Swift bridge to
/// Objective-C counterparts, like `String` and `Int`.
///
///     let s: AnyObject = "This is a bridged string."
///     print(s is NSString)
///     // Prints "true"
///
///     let v: AnyObject = 100
///     print(v.dynamicType)
///     // Prints "__NSCFNumber"
///
/// The flexible behavior of the `AnyObject` protocol is similar to
/// Objective-C's `id` type. For this reason, imported Objective-C types
/// frequently use `AnyObject` as the type for properties, method parameters,
/// and return values.
///
/// Casting AnyObject Instances to a Known Type
/// ===========================================
///
/// Objects with a concrete type of `AnyObject` maintain a specific dynamic
/// type and can be cast to that type using one of the type-cast operators
/// (`as`, `as?`, or `as!`).
///
/// In the code samples that follow, the elements of the `NSArray` instance
/// `numberObjects` have `AnyObject` as their type. The first example uses the
/// conditional downcast operator (`as?`) to conditionally cast the first
/// object in the `numberObjects` array to an instance of Swift's `String`
/// type.
///
///     let numberObjects: NSArray = ["one", "two", 3, 4]
///
///     let first: AnyObject = numberObjects[0]
///     if let first = first as? String {
///         print("The first object, '\(first)', is a String")
///     }
///     // Prints("The first object, 'one', is a String")
///
/// If you have prior knowledge that an `AnyObject` instance has a particular
/// type, you can use the unconditional downcast operator (`as!`). Performing
/// an invalid cast triggers a runtime error.
///
///     let second = numberObjects.object(at: 1) as! String
///     print("'\(second)' is also a String")
///     // Prints "'two' is also a String"
///
///     let badCase = numberObjects.object(at: 2) as! NSDate
///     // Runtime error
///
/// Casting is always safe in the context of a `switch` statement.
///
///     for object in numberObjects {
///         switch object {
///         case let x as String:
///             print("'\(x)' is a String")
///         default:
///             print("'\(object)' is not a String")
///         }
///     }
///     // Prints "'one' is a String"
///     // Prints "'two' is a String"
///     // Prints "'3' is not a String"
///     // Prints "'4' is not a String"
///
/// You can call a method that takes an `AnyObject` parameter with an instance
/// of any class, `@objc` protocol, or type that bridges to Objective-C. In
/// the following example, the `toFind` constant is of type `Int`, which
/// bridges to `NSNumber` when passed to an `NSArray` method that expects an
/// `AnyObject` parameter:
///
///     let toFind = 3
///     let i = numberObjects.index(of: toFind)
///     if i != NSNotFound {
///         print("Found '\(numberObjects[i])' at index \(i)")
///     } else {
///         print("Couldn't find \(toFind)")
///     }
///     // Prints "Found '3' at index 2"
///
/// Accessing Objective-C Methods and Properties
/// ============================================
///
/// When you use `AnyObject` as a concrete type, you have at your disposal
/// every `@objc` method and property---that is, methods and properties
/// imported from Objective-C or marked with the `@objc` attribute. Because
/// Swift can't guarantee at compile time that these methods and properties
/// are actually available on an `AnyObject` instance's underlying type, these
/// `@objc` symbols are available as implicitly unwrapped optional methods and
/// properties, respectively.
///
/// This example defines an `IntegerRef` type with an `@objc` method named
/// `getIntegerValue`.
///
///     class IntegerRef {
///         let value: Int
///         init(_ value: Int) {
///             self.value = value
///         }
///
///         @objc func getIntegerValue() -> Int {
///             return value
///         }
///     }
///
///     func getObject() -> AnyObject {
///         return IntegerRef(100)
///     }
///
///     let x: AnyObject = getObject()
///
/// In the example, `x` has a static type of `AnyObject` and a dynamic type of
/// `IntegerRef`. You can use optional chaining to call the `@objc` method
/// `getIntegerValue()` on `x` safely. If you're sure of the dynamic type of
/// `x`, you can call `getIntegerValue()` directly.
///
///     let possibleValue = x.getIntegerValue?()
///     print(possibleValue)
///     // Prints "Optional(100)"
///
///     let certainValue = x.getIntegerValue()
///     print(certainValue)
///     // Prints "100"
///
/// If the dynamic type of `x` doesn't implement a `getIntegerValue()` method,
/// the system returns a runtime error when you initialize `certainValue`.
///
/// Alternatively, if you need to test whether `x.getValue()` exists, use
/// optional binding before calling the method.
///
///     if let f = x.getIntegerValue {
///         print("The value of 'x' is \(f())")
///     } else {
///         print("'x' does not have a 'getIntegerValue()' method")
///     }
///     // Prints "The value of 'x' is 100"
///
/// - SeeAlso: `AnyClass`, `Any`
@objc
public protocol AnyObject : class {}
#else
/// The protocol to which all classes implicitly conform.
///
/// - SeeAlso: `AnyClass`
public protocol AnyObject : class {}
#endif
// Implementation note: the `AnyObject` protocol *must* not have any method or
// property requirements.

// FIXME: AnyObject should have an alternate version for non-objc without
// the @objc attribute, but AnyObject needs to be not be an address-only
// type to be able to be the target of castToNativeObject and an empty
// non-objc protocol appears not to be. There needs to be another way to make
// this the right kind of object.

/// The protocol to which all class types implicitly conform.
///
/// You can use the `AnyClass` protocol as the concrete type for an instance of
/// any class. When you do, all known `@objc` class methods and properties are
/// available as implicitly unwrapped optional methods and properties,
/// respectively. For example:
///
///     class IntegerRef {
///         @objc class func getDefaultValue() -> Int {
///             return 42
///         }
///     }
///
///     func getDefaultValue(_ c: AnyClass) -> Int? {
///         return c.getDefaultValue?()
///     }
///
/// The `getDefaultValue(_:)` function uses optional chaining to safely call
/// the implicitly unwrapped class method on `c`. Calling the function with
/// different class types shows how the `getDefaultValue()` class method is
/// only conditionally available.
///
///     print(getDefaultValue(IntegerRef.self))
///     // Prints "Optional(42)"
///
///     print(getDefaultValue(NSString.self))
///     // Prints "nil"
///
/// - SeeAlso: `AnyObject`, `Any`
public typealias AnyClass = AnyObject.Type

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

//
// Equatable
//

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
  func == (lhs: Self, rhs: Self) -> Bool
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

//
// Comparable
//

/// Returns a Boolean value indicating whether the value of the first argument
/// is greater than that of the second argument.
///
/// This is the default implementation of the greater-than operator (`>`) for
/// any type that conforms to `Comparable`.
///
/// - Parameters:
///   - lhs: A value to compare.
///   - rhs: Another value to compare.
public func > <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return rhs < lhs
}

/// Returns a Boolean value indicating whether the value of the first argument
/// is less than or equal to that of the second argument.
///
/// This is the default implementation of the less-than-or-equal-to
/// operator (`<=`) for any type that conforms to `Comparable`.
///
/// - Parameters:
///   - lhs: A value to compare.
///   - rhs: Another value to compare.
public func <= <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return !(rhs < lhs)
}

/// Returns a Boolean value indicating whether the value of the first argument
/// is greater than or equal to that of the second argument.
///
/// This is the default implementation of the greater-than-or-equal-to operator
/// (`>=`) for any type that conforms to `Comparable`.
///
/// - Parameters:
///   - lhs: A value to compare.
///   - rhs: Another value to compare.
/// - Returns: `true` if `lhs` is greater than or equal to `rhs`; otherwise,
///   `false`.
public func >= <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return !(lhs < rhs)
}

/// A type that can be compared using the relational operators `<`, `<=`, `>=`,
/// and `>`.
///
/// The `Comparable` protocol is used for types that have an inherent order,
/// such as numbers and strings. Many types in the standard library already
/// conform to the `Comparable` protocol. Add `Comparable` conformance to your
/// own custom types when you want to be able to compare instances using
/// relational operators or use standard library methods that are designed for
/// `Comparable` types.
///
/// The most familiar use of relational operators is to compare numbers, as in
/// the following example:
///
///     let currentTemp = 73
///
///     if currentTemp >= 90 {
///         print("It's a scorcher!")
///     } else if currentTemp < 65 {
///         print("Might need a sweater today.")
///     } else {
///         print("Seems like picnic weather!")
///     }
///     // Prints "Seems like picnic weather!"
///
/// You can use special versions of some sequence and collection operations
/// when working with a `Comparable` type. For example, if your array's
/// elements conform to `Comparable`, you can call the `sort()` method without
/// using arguments to sort the elements of your array in ascending order.
///
///     var measurements = [1.1, 1.5, 2.9, 1.2, 1.5, 1.3, 1.2]
///     measurements.sort()
///     print(measurements)
///     // Prints "[1.1, 1.2, 1.2, 1.3, 1.5, 1.5, 2.9]"
///
/// Conforming to the Comparable Protocol
/// =====================================
///
/// Types with Comparable conformance implement the less-than operator (`<`)
/// and the is-equal-to operator (`==`). These two operations impose a strict
/// total order on the values of a type, in which exactly one of the following
/// must be true for any two values `a` and `b`:
///
/// - `a == b`
/// - `a < b`
/// - `b < a`
///
/// In addition, the following conditions must hold:
///
/// - `a < a` is always `false` (Irreflexivity)
/// - `a < b` implies `!(b < a)` (Asymmetry)
/// - `a < b` and `b < c` implies `a < c` (Transitivity)
///
/// To add `Comparable` conformance to your custom types, define the `<` and
/// `==` operators. The `==` operator is a requirement of the `Equatable`
/// protocol, which `Comparable` extends---see that protocol's documentation
/// for more information about equality in Swift. Because default
/// implementations of the remainder of the relational operators are provided
/// by the standard library, you'll be able to use `!=`, `>`, `<=`, and `>=`
/// with instances of your type without any further code.
///
/// As an example, here's an implementation of a `Date` structure that stores
/// the year, month, and day of a date:
///
///     struct Date {
///         let year: Int
///         let month: Int
///         let day: Int
///     }
///
/// To add `Comparable` conformance to `Date`, first declare conformance to
/// `Comparable` and implement the `<` operator function.
///
///     extension Date: Comparable { }
///
///     func <(lhs: Date, rhs: Date) -> Bool {
///         if lhs.year != rhs.year {
///             return lhs.year < rhs.year
///         } else if lhs.month != rhs.month {
///             return lhs.month < rhs.month
///         } else {
///             return lhs.day < rhs.day
///         }
///     }
///
/// This function uses the least specific nonmatching property of the date to
/// determine the result of the comparison. For example, if the two `year`
/// properties are equal but the two `month` properties are not, the date with
/// the lesser value for `month` is the lesser of the two dates.
///
/// Next, implement the `==` operator function, the requirement inherited from
/// the `Equatable` protocol.
///
///     func ==(lhs: Date, rhs: Date) -> Bool {
///         return lhs.year == rhs.year && lhs.month == rhs.month
///             && lhs.day == rhs.day
///     }
///
/// Two `Date` instances are equal if each of their corresponding properties is
/// equal.
///
/// Now that `Date` conforms to `Comparable`, you can compare instances of the
/// type with any of the relational operators. The following example compares
/// the date of the first moon landing with the release of David Bowie's song
/// "Space Oddity":
///
///     let spaceOddity = Date(year: 1969, month: 7, day: 11)   // July 11, 1969
///     let moonLanding = Date(year: 1969, month: 7, day: 20)   // July 20, 1969
///     if moonLanding > spaceOddity {
///         print("Major Tom stepped through the door first.")
///     } else {
///         print("David Bowie was following in Neil Armstrong's footsteps.")
///     }
///     // Prints "Major Tom stepped through the door first."
///
/// Note that the `>` operator provided by the standard library is used in this
/// example, not the `<` operator implemented above.
///
/// - Note: A conforming type may contain a subset of values which are treated
///   as exceptional---that is, values that are outside the domain of
///   meaningful arguments for the purposes of the `Comparable` protocol. For
///   example, the special not-a-number (`FloatingPoint.nan`) value for
///   floating-point types compares as neither less than, greater than, nor
///   equal to any normal floating-point value. Exceptional values need not
///   take part in the strict total order.
public protocol Comparable : Equatable {
  /// Returns a Boolean value indicating whether the value of the first
  /// argument is less than that of the second argument.
  ///
  /// This function is the only requirement of the `Comparable` protocol. The
  /// remainder of the relational operator functions are implemented by the
  /// standard library for any type that conforms to `Comparable`.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  func < (lhs: Self, rhs: Self) -> Bool

  /// Returns a Boolean value indicating whether the value of the first
  /// argument is less than or equal to that of the second argument.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  func <= (lhs: Self, rhs: Self) -> Bool

  /// Returns a Boolean value indicating whether the value of the first
  /// argument is greater than or equal to that of the second argument.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  func >= (lhs: Self, rhs: Self) -> Bool

  /// Returns a Boolean value indicating whether the value of the first
  /// argument is greater than that of the second argument.
  ///
  /// - Parameters:
  ///   - lhs: A value to compare.
  ///   - rhs: Another value to compare.
  func > (lhs: Self, rhs: Self) -> Bool
}

/// A type that supports standard bitwise arithmetic operators.
///
/// Types that conform to the `BitwiseOperations` protocol implement operators
/// for bitwise arithmetic. The integer types in the standard library all
/// conform to `BitwiseOperations` by default. When you use bitwise operators
/// with an integer, you perform operations on the raw data bits that store
/// the integer's value.
///
/// In the following examples, the binary representation of any values are
/// shown in a comment to the right, like this:
///
///     let x: UInt8 = 5        // 0b00000101
///
/// Here are the required operators for the `BitwiseOperations` protocol:
///
/// - The bitwise OR operator (`|`) returns a value that has each bit set to
///   `1` where *one or both* of its arguments had that bit set to `1`. This
///   is equivalent to the union of two sets. For example:
///
///         let x: UInt8 = 5        // 0b00000101
///         let y: UInt8 = 14       // 0b00001110
///         let z = x | y           // 0b00001111
///
///   Performing a bitwise OR operation with a value and `allZeros` always
/// returns the same value.
///
///         print(x | .allZeros)    // 0b00000101
///         // Prints "5"
///
/// - The bitwise AND operator (`&`) returns a value that has each bit set to
///   `1` where *both* of its arguments had that bit set to `1`. This is
///   equivalent to the intersection of two sets. For example:
///
///         let x: UInt8 = 5        // 0b00000101
///         let y: UInt8 = 14       // 0b00001110
///         let z = x & y           // 0b00000100
///
///   Performing a bitwise AND operation with a value and `allZeros` always
/// returns `allZeros`.
///
///         print(x & .allZeros)    // 0b00000000
///         // Prints "0"
///
/// - The bitwise XOR operator (`^`), or exclusive OR operator, returns a value
///   that has each bit set to `1` where *one or the other but not both* of
///   its operators has that bit set to `1`. This is equivalent to the
///   symmetric difference of two sets. For example:
///
///         let x: UInt8 = 5        // 0b00000101
///         let y: UInt8 = 14       // 0b00001110
///         let z = x ^ y           // 0b00001011
///
///   Performing a bitwise XOR operation with a value and `allZeros` always
/// returns the same value.
///
///         print(x ^ .allZeros)    // 0b00000101
///         // Prints "5"
///
/// - The bitwise NOT operator (`~`) is a prefix operator that returns a value
///   where all the bits of its argument are flipped: Bits that are `1` in the
///   argument are `0` in the result, and bits that are `0` in the argument
///   are `1` in the result. This is equivalent to the inverse of a set. For
///   example:
///
///         let x: UInt8 = 5        // 0b00000101
///         let notX = ~x           // 0b11111010
///
///   Performing a bitwise NOT operation on `allZeros` returns a value with
///   every bit set to `1`.
///
///         let allOnes = ~UInt8.allZeros   // 0b11111111
///
/// The `OptionSet` protocol uses a raw value that conforms to
/// `BitwiseOperations` to provide mathematical set operations like
/// `union(_:)`, `intersection(_:)` and `contains(_:)` with O(1) performance.
///
/// Conforming to the BitwiseOperations Protocol
/// ============================================
///
/// To make your custom type conform to `BitwiseOperations`, add a static
/// `allZeros` property and declare the four required operator functions. Any
/// type that conforms to `BitwiseOperations`, where `x` is an instance of the
/// conforming type, must satisfy the following conditions:
///
/// - `x | Self.allZeros == x`
/// - `x ^ Self.allZeros == x`
/// - `x & Self.allZeros == .allZeros`
/// - `x & ~Self.allZeros == x`
/// - `~x == x ^ ~Self.allZeros`
///
/// - SeeAlso: `OptionSet`
public protocol BitwiseOperations {
  /// Returns the intersection of bits set in the two arguments.
  ///
  /// The bitwise AND operator (`&`) returns a value that has each bit set to
  /// `1` where *both* of its arguments had that bit set to `1`. This is
  /// equivalent to the intersection of two sets. For example:
  ///
  ///     let x: UInt8 = 5        // 0b00000101
  ///     let y: UInt8 = 14       // 0b00001110
  ///     let z = x & y           // 0b00000100
  ///
  /// Performing a bitwise AND operation with a value and `allZeros` always
  /// returns `allZeros`.
  ///
  ///     print(x & .allZeros)    // 0b00000000
  ///     // Prints "0"
  ///
  /// - Complexity: O(1).
  func & (lhs: Self, rhs: Self) -> Self

  /// Returns the union of bits set in the two arguments.
  ///
  /// The bitwise OR operator (`|`) returns a value that has each bit set to
  /// `1` where *one or both* of its arguments had that bit set to `1`. For
  /// example:
  ///
  ///     let x: UInt8 = 5        // 0b00000101
  ///     let y: UInt8 = 14       // 0b00001110
  ///     let z = x | y           // 0b00001111
  ///
  /// Performing a bitwise OR operation with a value and `allZeros` always
  /// returns the same value.
  ///
  ///     print(x | .allZeros)    // 0b00000101
  ///     // Prints "5"
  ///
  /// - Complexity: O(1).
  func | (lhs: Self, rhs: Self) -> Self

  /// Returns the bits that are set in exactly one of the two arguments.
  ///
  /// The bitwise XOR operator (`^`), or exclusive OR operator, returns a value
  /// that has each bit set to `1` where *one or the other but not both* of
  /// its operators has that bit set to `1`. This is equivalent to the
  /// symmetric difference of two sets. For example:
  ///
  ///     let x: UInt8 = 5        // 0b00000101
  ///     let y: UInt8 = 14       // 0b00001110
  ///     let z = x ^ y           // 0b00001011
  ///
  /// Performing a bitwise XOR with a value and `allZeros` always returns the
  /// same value:
  ///
  ///     print(x ^ .allZeros)    // 0b00000101
  ///     // Prints "5"
  ///
  /// - Complexity: O(1).
  func ^ (lhs: Self, rhs: Self) -> Self

  /// Returns the inverse of the bits set in the argument.
  ///
  /// The bitwise NOT operator (`~`) is a prefix operator that returns a value
  /// in which all the bits of its argument are flipped: Bits that are `1` in the
  /// argument are `0` in the result, and bits that are `0` in the argument
  /// are `1` in the result. This is equivalent to the inverse of a set. For
  /// example:
  ///
  ///     let x: UInt8 = 5        // 0b00000101
  ///     let notX = ~x           // 0b11111010
  ///
  /// Performing a bitwise NOT operation on `allZeros` returns a value with
  /// every bit set to `1`.
  ///
  ///     let allOnes = ~UInt8.allZeros   // 0b11111111
  ///
  /// - Complexity: O(1).
  prefix func ~ (x: Self) -> Self

  /// The empty bitset.
  ///
  /// The `allZeros` static property is the [identity element][] for bitwise OR
  /// and XOR operations and the [fixed point][] for bitwise AND operations.
  /// For example:
  ///
  ///     let x: UInt8 = 5        // 0b00000101
  ///
  ///     // Identity
  ///     x | .allZeros           // 0b00000101
  ///     x ^ .allZeros           // 0b00000101
  ///
  ///     // Fixed point
  ///     x & .allZeros           // 0b00000000
  ///
  /// [identity element]:http://en.wikipedia.org/wiki/Identity_element
  /// [fixed point]:http://en.wikipedia.org/wiki/Fixed_point_(mathematics)
  static var allZeros: Self { get }
}

/// Calculates the union of bits sets in the two arguments and stores the result
/// in the first argument.
///
/// - Parameters:
///   - lhs: A value to update with the union of bits set in the two arguments.
///   - rhs: Another value.
public func |= <T : BitwiseOperations>(lhs: inout T, rhs: T) {
  lhs = lhs | rhs
}

/// Calculates the intersections of bits sets in the two arguments and stores
/// the result in the first argument.
///
/// - Parameters:
///   - lhs: A value to update with the intersections of bits set in the two
///     arguments.
///   - rhs: Another value.
public func &= <T : BitwiseOperations>(lhs: inout T, rhs: T) {
  lhs = lhs & rhs
}

/// Calculates the bits that are set in exactly one of the two arguments and
/// stores the result in the first argument.
///
/// - Parameters:
///   - lhs: A value to update with the bits that are set in exactly one of the
///     two arguments.
///   - rhs: Another value.
public func ^= <T : BitwiseOperations>(lhs: inout T, rhs: T) {
  lhs = lhs ^ rhs
}

/// A type that provides an integer hash value.
///
/// You can use any type that conforms to the `Hashable` protocol in a set or
/// as a dictionary key. Many types in the standard library conform to
/// `Hashable`: strings, integers, floating-point and Boolean values, and even
/// sets provide a hash value by default. Your own custom types can be
/// hashable as well. When you define an enumeration without associated
/// values, it gains `Hashable` conformance automatically, and you can add
/// `Hashable` conformance to your other custom types by adding a single
/// `hashValue` property.
///
/// A hash value, provided by a type's `hashValue` property, is an integer that
/// is the same for any two instances that compare equally. That is, for two
/// instances `a` and `b` of the same type, if `a == b` then
/// `a.hashValue == b.hashValue`. The reverse is not true: Two instances with
/// equal hash values are not necessarily equal to each other.
///
/// - Important: Hash values are not guaranteed to be equal across different
///   executions of your program. Do not save hash values to use during a
///   future execution.
///
/// Conforming to the Hashable Protocol
/// ===================================
///
/// To use your own custom type in a set or as the key type of a dictionary,
/// add `Hashable` conformance to your type by providing a `hashValue`
/// property. The `Hashable` protocol inherits from the `Equatable` protocol,
/// so you must also add an is-equal-to operator (`==`) function for your
/// custom type.
///
/// As an example, consider a `GridPoint` type that describes a location in a
/// grid of buttons. Here's the initial declaration of the `GridPoint` type:
///
///     /// A point in an x-y coordinate system.
///     struct GridPoint {
///         var x: Int
///         var y: Int
///     }
///
/// You'd like to create a set of the grid points where a user has already
/// tapped. Because the `GridPoint` type is not hashable yet, it can't be used
/// as the `Element` type for a set. To add `Hashable` conformance, provide an
/// `==` operator function and a `hashValue` property.
///
///     func ==(lhs: GridPoint, rhs: GridPoint) -> Bool {
///         return lhs.x == rhs.x && lhs.y == rhs.y
///     }
///
///     extension GridPoint: Hashable {
///         var hashValue: Int {
///             return x.hashValue ^ y.hashValue
///         }
///     }
///
/// The `hashValue` property in this example combines the hash values of a grid
/// point's `x` and `y` values using the bitwise XOR operator (`^`). The `^`
/// operator is one way to combine two integer values into a single value.
///
/// - Note: Set and dictionary performance depends on hash values that minimize
///   collisions for their associated element and key types, respectively.
///
/// Now that `GridPoint` conforms to the `Hashable` protocol, you can create a
/// set of previously tapped grid points.
///
///     var tappedPoints: Set = [GridPoint(x: 2, y: 3), GridPoint(x: 4, y: 1)]
///     let nextTap = GridPoint(x: 0, y: 1)
///     if tappedPoints.contains(nextTap) {
///         print("Already tapped at (\(nextTap.x), \(nextTap.y)).")
///     } else {
///         tappedPoints.insert(nextTap)
///         print("New tap detected at (\(nextTap.x), \(nextTap.y)).")
///     }
///     // Prints "New tap detected at (0, 1).")
public protocol Hashable : Equatable {
  /// The hash value.
  ///
  /// Hash values are not guaranteed to be equal across different executions of
  /// your program. Do not save hash values to use during a future execution.
  var hashValue: Int { get }
}

//===----------------------------------------------------------------------===//
// Standard pattern matching forms
//===----------------------------------------------------------------------===//

// Equatable types can be matched in patterns by value equality.
@_transparent
public func ~= <T : Equatable> (a: T, b: T) -> Bool {
  return a == b
}

//===----------------------------------------------------------------------===//
// Standard operators
//===----------------------------------------------------------------------===//

// Standard postfix operators.
postfix operator ++ {}
postfix operator -- {}

// Optional<T> unwrapping operator is built into the compiler as a part of
// postfix expression grammar.
//
// postfix operator ! {}

// Standard prefix operators.
prefix operator ++ {}
prefix operator -- {}
prefix operator ! {}
prefix operator ~ {}
prefix operator + {}
prefix operator - {}

// Standard infix operators.

// "Exponentiative"

infix operator << { associativity none precedence 160 }
infix operator >> { associativity none precedence 160 }

// "Multiplicative"

infix operator   * { associativity left precedence 150 }
infix operator  &* { associativity left precedence 150 }
infix operator   / { associativity left precedence 150 }
infix operator   % { associativity left precedence 150 }
infix operator   & { associativity left precedence 150 }

// "Additive"

infix operator   + { associativity left precedence 140 }
infix operator  &+ { associativity left precedence 140 }
infix operator   - { associativity left precedence 140 }
infix operator  &- { associativity left precedence 140 }
infix operator   | { associativity left precedence 140 }
infix operator   ^ { associativity left precedence 140 }

// FIXME: is this the right precedence level for "..." ?
infix operator  ... { associativity none precedence 135 }
infix operator  ..< { associativity none precedence 135 }

// The cast operators 'as' and 'is' are hardcoded as if they had the
// following attributes:
// infix operator as { associativity none precedence 132 }

// "Coalescing"
infix operator ?? { associativity right precedence 131 }

// "Comparative"

infix operator  <  { associativity none precedence 130 }
infix operator  <= { associativity none precedence 130 }
infix operator  >  { associativity none precedence 130 }
infix operator  >= { associativity none precedence 130 }
infix operator  == { associativity none precedence 130 }
infix operator  != { associativity none precedence 130 }
infix operator === { associativity none precedence 130 }
infix operator !== { associativity none precedence 130 }
// FIXME: ~= will be built into the compiler.
infix operator  ~= { associativity none precedence 130 }

// "Conjunctive"

infix operator && { associativity left precedence 120 }

// "Disjunctive"

infix operator || { associativity left precedence 110 }


// User-defined ternary operators are not supported. The ? : operator is
// hardcoded as if it had the following attributes:
// operator ternary ? : { associativity right precedence 100 }

// User-defined assignment operators are not supported. The = operator is
// hardcoded as if it had the following attributes:
// infix operator = { associativity right precedence 90 }

// Compound

infix operator  *= { associativity right precedence 90 assignment }
infix operator  /= { associativity right precedence 90 assignment }
infix operator  %= { associativity right precedence 90 assignment }
infix operator  += { associativity right precedence 90 assignment }
infix operator  -= { associativity right precedence 90 assignment }
infix operator <<= { associativity right precedence 90 assignment }
infix operator >>= { associativity right precedence 90 assignment }
infix operator  &= { associativity right precedence 90 assignment }
infix operator  ^= { associativity right precedence 90 assignment }
infix operator  |= { associativity right precedence 90 assignment }

// Workaround for <rdar://problem/14011860> SubTLF: Default
// implementations in protocols.  Library authors should ensure
// that this operator never needs to be seen by end-users.  See
// test/Prototypes/GenericDispatch.swift for a fully documented
// example of how this operator is used, and how its use can be hidden
// from users.
infix operator ~> { associativity left precedence 255 }

@available(*, unavailable, renamed: "BitwiseOperations")
public typealias BitwiseOperationsType = BitwiseOperations

