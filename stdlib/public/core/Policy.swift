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
/// `as?` (conditional downcast) operator to conditionally cast the first
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
/// type, you can use the `as!` (unconditional downcast) operator. Performing
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
@warn_unused_result
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

@warn_unused_result
public func !== (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
  return !(lhs === rhs)
}

//
// Equatable
//

/// Instances of conforming types can be compared for value equality
/// using operators `==` and `!=`.
///
/// When adopting `Equatable`, only the `==` operator is required to be
/// implemented.  The standard library provides an implementation for `!=`.
public protocol Equatable {
  /// Returns `true` if `lhs` is equal to `rhs`.
  ///
  /// **Equality implies substitutability**.  When `x == y`, `x` and
  /// `y` are interchangeable in any code that only depends on their
  /// values.
  ///
  /// Class instance identity as distinguished by triple-equals `===`
  /// is notably not part of an instance's value.  Exposing other
  /// non-value aspects of `Equatable` types is discouraged, and any
  /// that *are* exposed should be explicitly pointed out in
  /// documentation.
  ///
  /// **Equality is an equivalence relation**
  ///
  /// - `x == x` is `true`
  /// - `x == y` implies `y == x`
  /// - `x == y` and `y == z` implies `x == z`
  ///
  /// **Inequality is the inverse of equality**, i.e. `!(x == y)` iff
  /// `x != y`.
  @warn_unused_result
  func == (lhs: Self, rhs: Self) -> Bool
}

@warn_unused_result
public func != <T : Equatable>(lhs: T, rhs: T) -> Bool {
  return !(lhs == rhs)
}

//
// Comparable
//

@warn_unused_result
public func > <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return rhs < lhs
}

@warn_unused_result
public func <= <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return !(rhs < lhs)
}

@warn_unused_result
public func >= <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return !(lhs < rhs)
}

/// Instances of conforming types can be compared using relational
/// operators, which define a [strict total order](http://en.wikipedia.org/wiki/Total_order#Strict_total_order)
/// on normal values. A conforming type may contain a subset of values which
/// are treated as exceptional; i.e. outside the domain of meaningful arguments
/// for the purposes of the Comparable protocol; such values need not take part
/// in the strict total order. One example is NaN for floating-point types.
///
/// A type conforming to `Comparable` need only supply the `<` and
/// `==` operators; default implementations of `<=`, `>`, `>=`, and
/// `!=` are supplied by the standard library:
///
///     struct Singular : Comparable {}
///     func ==(x: Singular, y: Singular) -> Bool { return true }
///     func <(x: Singular, y: Singular) -> Bool { return false }
///
/// **Axioms**, in addition to those of `Equatable`:
///
/// - `x == y` implies `x <= y`, `x >= y`, `!(x < y)`, and `!(x > y)`
/// - `x < y` implies `x <= y` and `y > x`
/// - `x > y` implies `x >= y` and `y < x`
/// - `x <= y` implies `y >= x`
/// - `x >= y` implies `y <= x`
public protocol Comparable : Equatable {
  /// A [strict total order](http://en.wikipedia.org/wiki/Total_order#Strict_total_order)
  /// over instances of `Self`.
  @warn_unused_result
  func < (lhs: Self, rhs: Self) -> Bool

  @warn_unused_result
  func <= (lhs: Self, rhs: Self) -> Bool

  @warn_unused_result
  func >= (lhs: Self, rhs: Self) -> Bool

  @warn_unused_result
  func > (lhs: Self, rhs: Self) -> Bool
}

/// A set type with O(1) standard bitwise operators.
///
/// Each instance is a subset of `~Self.allZeros`.
///
/// **Axioms**, where `x` is an instance of `Self`:
///
/// -  `x | Self.allZeros == x`
/// -  `x ^ Self.allZeros == x`
/// -  `x & Self.allZeros == .allZeros`
/// -  `x & ~Self.allZeros == x`
/// -  `~x == x ^ ~Self.allZeros`
public protocol BitwiseOperations {
  /// Returns the intersection of bits set in `lhs` and `rhs`.
  ///
  /// - Complexity: O(1).
  @warn_unused_result
  func & (lhs: Self, rhs: Self) -> Self

  /// Returns the union of bits set in `lhs` and `rhs`.
  ///
  /// - Complexity: O(1).
  @warn_unused_result
  func | (lhs: Self, rhs: Self) -> Self

  /// Returns the bits that are set in exactly one of `lhs` and `rhs`.
  ///
  /// - Complexity: O(1).
  @warn_unused_result
  func ^ (lhs: Self, rhs: Self) -> Self

  /// Returns `x ^ ~Self.allZeros`.
  ///
  /// - Complexity: O(1).
  @warn_unused_result
  prefix func ~ (x: Self) -> Self

  /// The empty bitset.
  ///
  /// Also the [identity element](http://en.wikipedia.org/wiki/Identity_element) for `|` and
  /// `^`, and the [fixed point](http://en.wikipedia.org/wiki/Fixed_point_(mathematics)) for
  /// `&`.
  static var allZeros: Self { get }
}

public func |= <T : BitwiseOperations>(lhs: inout T, rhs: T) {
  lhs = lhs | rhs
}

public func &= <T : BitwiseOperations>(lhs: inout T, rhs: T) {
  lhs = lhs & rhs
}

public func ^= <T : BitwiseOperations>(lhs: inout T, rhs: T) {
  lhs = lhs ^ rhs
}

/// Instances of conforming types provide an integer `hashValue` and
/// can be used as `Dictionary` keys.
public protocol Hashable : Equatable {
  /// The hash value.
  ///
  /// **Axiom:** `x == y` implies `x.hashValue == y.hashValue`.
  ///
  /// - Note: The hash value is not guaranteed to be stable across
  ///   different invocations of the same program.  Do not persist the
  ///   hash value across program runs.
  var hashValue: Int { get }
}

//===----------------------------------------------------------------------===//
// Standard pattern matching forms
//===----------------------------------------------------------------------===//

// Equatable types can be matched in patterns by value equality.
@_transparent
@warn_unused_result
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

