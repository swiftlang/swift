//===----------------------------------------------------------------------===//
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
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Standardized aliases
//===----------------------------------------------------------------------===//
/// The empty tuple type.
///
/// This is the default return type of functions for which no explicit
/// return type is specified.
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
public typealias BooleanLiteralType = Bool
// typealias CharacterLiteralType = ?
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
#if arch(i386) || arch(x86_64)
public typealias _MaxBuiltinFloatType = Builtin.FPIEEE80
#else
public typealias _MaxBuiltinFloatType = Builtin.FPIEEE64
#endif

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

/// The protocol to which all types implicitly conform.
public typealias Any = protocol<>

/// The protocol to which all classes implicitly conform.
///
/// When used as a concrete type, all known `@objc` methods and
/// properties are available, as implicitly-unwrapped-optional methods
/// and properties respectively, on each instance of `AnyObject`.  For
/// example:
///
///     class C {
///       @objc func getCValue() -> Int { return 42 }
///     }
///
///     // If x has a method @objc getValue()->Int, call it and
///     // return the result.  Otherwise, return nil.
///     func getCValue1(x: AnyObject) -> Int? {
///       if let f: ()->Int = x.getCValue { // <===
///         return f()
///       }
///       return nil
///     }
///
///     // A more idiomatic implementation using "optional chaining"
///     func getCValue2(x: AnyObject) -> Int? {
///       return x.getCValue?() // <===
///     }
///
///     // An implementation that assumes the required method is present
///     func getCValue3(x: AnyObject) -> Int { // <===
///       return x.getCValue() // x.getCValue is implicitly unwrapped. // <===
///     }
///
/// This protocol *must* not have any method or property requirements.
/// (Final extension methods are OK though.)
///
/// - SeeAlso: `AnyClass`
#if _runtime(_ObjC)
@objc public protocol AnyObject: class {}
#else
public protocol AnyObject: class {}
#endif

// FIXME: AnyObject should have an alternate version for non-objc without
// the @objc attribute, but AnyObject needs to be not be an address-only
// type to be able to be the target of castToNativeObject and an empty
// non-objc protocol appears not to be. There needs to be another way to make
// this the right kind of object.

/// The protocol to which all class types implicitly conform.
///
/// When used as a concrete type, all known `@objc` `class` methods and
/// properties are available, as implicitly-unwrapped-optional methods
/// and properties respectively, on each instance of `AnyClass`. For
/// example:
///
///     class C {
///       @objc class var cValue: Int { return 42 }
///     }
///
///     // If x has an @objc cValue: Int, return its value.
///     // Otherwise, return nil.
///     func getCValue(x: AnyClass) -> Int? {
///       return x.cValue // <===
///     }
///
/// - SeeAlso: `AnyObject`
public typealias AnyClass = AnyObject.Type

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

/// Instances of conforming types can be compared for value equality
/// using operators `==` and `!=`.
///
/// When adopting `Equatable`, only the `==` operator is required to be
/// implemented.  The standard library provides an implementation for `!=`.
public protocol Equatable {
  /// Return true if `lhs` is equal to `rhs`.
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
  func == (lhs: Self, rhs: Self) -> Bool
}

public func != <T : Equatable>(lhs: T, rhs: T) -> Bool {
  return !(lhs == rhs)
}

//
// Comparable
//

public func > <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return rhs < lhs
}
public func <= <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return !(rhs < lhs)
}
public func >= <T : Comparable>(lhs: T, rhs: T) -> Bool {
  return !(lhs < rhs)
}

/// Instances of conforming types can be compared using relational
/// operators, which define a [strict total order](http://en.wikipedia.org/wiki/Total_order#Strict_total_order).
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
  func <(lhs: Self, rhs: Self) -> Bool
  func <=(lhs: Self, rhs: Self) -> Bool
  func >=(lhs: Self, rhs: Self) -> Bool
  func >(lhs: Self, rhs: Self) -> Bool
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
public protocol BitwiseOperationsType {
  /// Returns the intersection of bits set in `lhs` and `rhs`.
  ///
  /// - Complexity: O(1).
  func & (lhs: Self, rhs: Self) -> Self

  /// Returns the union of bits set in `lhs` and `rhs`.
  ///
  /// - Complexity: O(1).
  func |(lhs: Self, rhs: Self) -> Self

  /// Returns the bits that are set in exactly one of `lhs` and `rhs`.
  ///
  /// - Complexity: O(1).
  func ^(lhs: Self, rhs: Self) -> Self

  /// Returns `x ^ ~Self.allZeros`.
  ///
  /// - Complexity: O(1).
  prefix func ~(x: Self) -> Self

  /// The empty bitset.
  ///
  /// Also the [identity element](http://en.wikipedia.org/wiki/Identity_element) for `|` and
  /// `^`, and the [fixed point](http://en.wikipedia.org/wiki/Fixed_point_(mathematics)) for
  /// `&`.
  static var allZeros: Self { get }
}

public func |= <T : BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs | rhs
}

public func &= <T : BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs & rhs
}

public func ^= <T : BitwiseOperationsType>(inout lhs: T, rhs: T) {
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

/// Instances of conforming types are effectively functions with the
/// signature `(Element) -> Void`.
///
/// Useful mainly when the optimizer's ability to specialize generics
/// outstrips its ability to specialize ordinary closures.  For
/// example, you may find that instead of:
///
///     func f(g: (X)->Void) { ... g(a) ...}
///
/// the following generates better code:
///
///     func f<T : SinkType where T.Element == X>(g: T) { ... g.put(a) ...}
public protocol SinkType {
  /// The type of element to be written to this sink.
  typealias Element
  /// Write `x` to this sink.
  mutating func put(x: Element)
}

//===----------------------------------------------------------------------===//
// Standard pattern matching forms
//===----------------------------------------------------------------------===//

// Equatable types can be matched in patterns by value equality.
@transparent
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

