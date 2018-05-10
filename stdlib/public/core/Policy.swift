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
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Standardized uninhabited type
//===----------------------------------------------------------------------===//
/// The return type of functions that do not return normally, that is, a type
/// with no values.
///
/// Use `Never` as the return type when declaring a closure, function, or
/// method that unconditionally throws an error, traps, or otherwise does
/// not terminate.
///
///     func crashAndBurn() -> Never {
///         fatalError("Something very, very bad happened")
///     }
@_frozen
public enum Never {}

//===----------------------------------------------------------------------===//
// Standardized aliases
//===----------------------------------------------------------------------===//
/// The return type of functions that don't explicitly specify a return type,
/// that is, an empty tuple `()`.
///
/// When declaring a function or method, you don't need to specify a return
/// type if no value will be returned. However, the type of a function,
/// method, or closure always includes a return type, which is `Void` if
/// otherwise unspecified.
///
/// Use `Void` or an empty tuple as the return type when declaring a closure,
/// function, or method that doesn't return a value.
///
///     // No return type declared:
///     func logMessage(_ s: String) {
///         print("Message: \(s)")
///     }
///
///     let logger: (String) -> Void = logMessage
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
///     print("isBool is a '\(type(of: isBool))'")
///     // Prints "isBool is a 'Bool'"
///
/// The type aliased by `BooleanLiteralType` must conform to the
/// `ExpressibleByBooleanLiteral` protocol.
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
///     let s: AnyObject = "This is a bridged string." as NSString
///     print(s is NSString)
///     // Prints "true"
///
///     let v: AnyObject = 100 as NSNumber
///     print(type(of: v))
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
/// This example uses the conditional downcast operator (`as?`) to
/// conditionally cast the `s` constant declared above to an instance of
/// Swift's `String` type.
///
///     if let message = s as? String {
///         print("Successful cast to String: \(message)")
///     }
///     // Prints "Successful cast to String: This is a bridged string."
///
/// If you have prior knowledge that an `AnyObject` instance has a particular
/// type, you can use the unconditional downcast operator (`as!`). Performing
/// an invalid cast triggers a runtime error.
///
///     let message = s as! String
///     print("Successful cast to String: \(message)")
///     // Prints "Successful cast to String: This is a bridged string."
///
///     let badCase = v as! String
///     // Runtime error
///
/// Casting is always safe in the context of a `switch` statement.
///
///     let mixedArray: [AnyObject] = [s, v]
///     for object in mixedArray {
///         switch object {
///         case let x as String:
///             print("'\(x)' is a String")
///         default:
///             print("'\(object)' is not a String")
///         }
///     }
///     // Prints "'This is a bridged string.' is a String"
///     // Prints "'100' is not a String"
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
/// `getIntegerValue()`.
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
///     let obj: AnyObject = getObject()
///
/// In the example, `obj` has a static type of `AnyObject` and a dynamic type
/// of `IntegerRef`. You can use optional chaining to call the `@objc` method
/// `getIntegerValue()` on `obj` safely. If you're sure of the dynamic type of
/// `obj`, you can call `getIntegerValue()` directly.
///
///     let possibleValue = obj.getIntegerValue?()
///     print(possibleValue)
///     // Prints "Optional(100)"
///
///     let certainValue = obj.getIntegerValue()
///     print(certainValue)
///     // Prints "100"
///
/// If the dynamic type of `obj` doesn't implement a `getIntegerValue()`
/// method, the system returns a runtime error when you initialize
/// `certainValue`.
///
/// Alternatively, if you need to test whether `obj.getIntegerValue()` exists,
/// use optional binding before calling the method.
///
///     if let f = obj.getIntegerValue {
///         print("The value of 'obj' is \(f())")
///     } else {
///         print("'obj' does not have a 'getIntegerValue()' method")
///     }
///     // Prints "The value of 'obj' is 100"
public typealias AnyObject = Builtin.AnyObject
#else
/// The protocol to which all classes implicitly conform.
public typealias AnyObject = Builtin.AnyObject
#endif

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
public typealias AnyClass = AnyObject.Type

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
///   returns the same value.
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
///   returns `allZeros`.
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
///   returns the same value.
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
@available(swift, deprecated: 3.1, obsoleted: 4.0, message: "Use FixedWidthInteger protocol instead")
public typealias BitwiseOperations = _BitwiseOperations

public protocol _BitwiseOperations {
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
  static func & (lhs: Self, rhs: Self) -> Self

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
  static func | (lhs: Self, rhs: Self) -> Self

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
  static func ^ (lhs: Self, rhs: Self) -> Self

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
  static prefix func ~ (x: Self) -> Self

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
  @available(swift, deprecated: 3.1, obsoleted: 4.0, message: "Use 0 or init() of a type conforming to FixedWidthInteger")
  static var allZeros: Self { get }
}

extension _BitwiseOperations {
  /// Calculates the union of bits sets in the two arguments and stores the result
  /// in the first argument.
  ///
  /// - Parameters:
  ///   - lhs: A value to update with the union of bits set in the two arguments.
  ///   - rhs: Another value.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4.1)
  public static func |= (lhs: inout Self, rhs: Self) {
    lhs = lhs | rhs
  }

  /// Calculates the intersections of bits sets in the two arguments and stores
  /// the result in the first argument.
  ///
  /// - Parameters:
  ///   - lhs: A value to update with the intersections of bits set in the two
  ///     arguments.
  ///   - rhs: Another value.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4.1)
  public static func &= (lhs: inout Self, rhs: Self) {
    lhs = lhs & rhs
  }

  /// Calculates the bits that are set in exactly one of the two arguments and
  /// stores the result in the first argument.
  ///
  /// - Parameters:
  ///   - lhs: A value to update with the bits that are set in exactly one of the
  ///     two arguments.
  ///   - rhs: Another value.
  @inlinable // FIXME(sil-serialize-all)
  @available(swift, obsoleted: 4.1)
  public static func ^= (lhs: inout Self, rhs: Self) {
    lhs = lhs ^ rhs
  }
}

//===----------------------------------------------------------------------===//
// Standard pattern matching forms
//===----------------------------------------------------------------------===//

/// Returns a Boolean value indicating whether two arguments match by value
/// equality.
///
/// The pattern-matching operator (`~=`) is used internally in `case`
/// statements for pattern matching. When you match against an `Equatable`
/// value in a `case` statement, this operator is called behind the scenes.
///
///     let weekday = 3
///     let lunch: String
///     switch weekday {
///     case 3:
///         lunch = "Taco Tuesday!"
///     default:
///         lunch = "Pizza again."
///     }
///     // lunch == "Taco Tuesday!"
///
/// In this example, the `case 3` expression uses this pattern-matching
/// operator to test whether `weekday` is equal to the value `3`.
///
/// - Note: In most cases, you should use the equal-to operator (`==`) to test
///   whether two instances are equal. The pattern-matching operator is
///   primarily intended to enable `case` statement pattern matching.
///
/// - Parameters:
///   - lhs: A value to compare.
///   - rhs: Another value to compare.
@inlinable // FIXME(sil-serialize-all)
@_transparent
public func ~= <T : Equatable>(a: T, b: T) -> Bool {
  return a == b
}

//===----------------------------------------------------------------------===//
// Standard precedence groups
//===----------------------------------------------------------------------===//

precedencegroup AssignmentPrecedence {
  assignment: true
  associativity: right
}
precedencegroup FunctionArrowPrecedence {
  associativity: right
  higherThan: AssignmentPrecedence
}
precedencegroup TernaryPrecedence {
  associativity: right
  higherThan: FunctionArrowPrecedence
}
precedencegroup DefaultPrecedence {
  higherThan: TernaryPrecedence
}
precedencegroup LogicalDisjunctionPrecedence {
  associativity: left
  higherThan: TernaryPrecedence
}
precedencegroup LogicalConjunctionPrecedence {
  associativity: left
  higherThan: LogicalDisjunctionPrecedence
}
precedencegroup ComparisonPrecedence {
  higherThan: LogicalConjunctionPrecedence
}
precedencegroup NilCoalescingPrecedence {
  associativity: right
  higherThan: ComparisonPrecedence
}
precedencegroup CastingPrecedence {
  higherThan: NilCoalescingPrecedence
}
precedencegroup RangeFormationPrecedence {
  higherThan: CastingPrecedence
}
precedencegroup AdditionPrecedence {
  associativity: left
  higherThan: RangeFormationPrecedence
}
precedencegroup MultiplicationPrecedence {
  associativity: left
  higherThan: AdditionPrecedence
}
precedencegroup BitwiseShiftPrecedence {
  higherThan: MultiplicationPrecedence
}


//===----------------------------------------------------------------------===//
// Standard operators
//===----------------------------------------------------------------------===//

// Standard postfix operators.
postfix operator ++
postfix operator --
postfix operator ...

// Optional<T> unwrapping operator is built into the compiler as a part of
// postfix expression grammar.
//
// postfix operator !

// Standard prefix operators.
prefix operator ++
prefix operator --
prefix operator !
prefix operator ~
prefix operator +
prefix operator -
prefix operator ...
prefix operator ..<

// Standard infix operators.

// "Exponentiative"

infix operator  << : BitwiseShiftPrecedence
infix operator &<< : BitwiseShiftPrecedence
infix operator  >> : BitwiseShiftPrecedence
infix operator &>> : BitwiseShiftPrecedence

// "Multiplicative"

infix operator   * : MultiplicationPrecedence
infix operator  &* : MultiplicationPrecedence
infix operator   / : MultiplicationPrecedence
infix operator   % : MultiplicationPrecedence
infix operator   & : MultiplicationPrecedence

// "Additive"

infix operator   + : AdditionPrecedence
infix operator  &+ : AdditionPrecedence
infix operator   - : AdditionPrecedence
infix operator  &- : AdditionPrecedence
infix operator   | : AdditionPrecedence
infix operator   ^ : AdditionPrecedence

// FIXME: is this the right precedence level for "..." ?
infix operator  ... : RangeFormationPrecedence
infix operator  ..< : RangeFormationPrecedence

// The cast operators 'as' and 'is' are hardcoded as if they had the
// following attributes:
// infix operator as : CastingPrecedence

// "Coalescing"

infix operator ?? : NilCoalescingPrecedence

// "Comparative"

infix operator  <  : ComparisonPrecedence
infix operator  <= : ComparisonPrecedence
infix operator  >  : ComparisonPrecedence
infix operator  >= : ComparisonPrecedence
infix operator  == : ComparisonPrecedence
infix operator  != : ComparisonPrecedence
infix operator === : ComparisonPrecedence
infix operator !== : ComparisonPrecedence
// FIXME: ~= will be built into the compiler.
infix operator  ~= : ComparisonPrecedence

// "Conjunctive"

infix operator && : LogicalConjunctionPrecedence

// "Disjunctive"

infix operator || : LogicalDisjunctionPrecedence

// User-defined ternary operators are not supported. The ? : operator is
// hardcoded as if it had the following attributes:
// operator ternary ? : : TernaryPrecedence

// User-defined assignment operators are not supported. The = operator is
// hardcoded as if it had the following attributes:
// infix operator = : AssignmentPrecedence

// Compound

infix operator   *= : AssignmentPrecedence
infix operator  &*= : AssignmentPrecedence
infix operator   /= : AssignmentPrecedence
infix operator   %= : AssignmentPrecedence
infix operator   += : AssignmentPrecedence
infix operator  &+= : AssignmentPrecedence
infix operator   -= : AssignmentPrecedence
infix operator  &-= : AssignmentPrecedence
infix operator  <<= : AssignmentPrecedence
infix operator &<<= : AssignmentPrecedence
infix operator  >>= : AssignmentPrecedence
infix operator &>>= : AssignmentPrecedence
infix operator   &= : AssignmentPrecedence
infix operator   ^= : AssignmentPrecedence
infix operator   |= : AssignmentPrecedence

// Workaround for <rdar://problem/14011860> SubTLF: Default
// implementations in protocols.  Library authors should ensure
// that this operator never needs to be seen by end-users.  See
// test/Prototypes/GenericDispatch.swift for a fully documented
// example of how this operator is used, and how its use can be hidden
// from users.
infix operator ~>
