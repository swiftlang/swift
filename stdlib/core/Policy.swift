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
public typealias Void = ()

//===----------------------------------------------------------------------===//
// Aliases for floating point types
//===----------------------------------------------------------------------===//
// FIXME: it should be the other way round, Float = Float32, Double = Float64,
// but the type checker loses sugar currently, and ends up displaying 'FloatXX'
// in diagnostics.
public typealias Float32 = Float
public typealias Float64 = Double

//===----------------------------------------------------------------------===//
// Default types for unconstrained literals
//===----------------------------------------------------------------------===//
public typealias IntegerLiteralType = Int
public typealias FloatLiteralType = Double
public typealias BooleanLiteralType = Bool
// typealias CharacterLiteralType = ?
public typealias UnicodeScalarType = String
public typealias ExtendedGraphemeClusterType = String
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

/// The protocol to which all types implicitly conform
public typealias Any = protocol<>

/// The protocol to which all classes implicitly conform.
///
/// When used as a concrete type, all known `@objc` methods and
/// properties are available, as implicitly-unwrapped-optional methods
/// and properties respectively, on each instance of `AnyObject`.  For
/// example:
///
/// .. parsed-literal:
///
///   class C {
///     @objc func getCValue() -> Int { return 42 }
///   }
///
///   // If x has a method @objc getValue()->Int, call it and
///   // return the result.  Otherwise, return nil.
///   func getCValue1(x: AnyObject) -> Int? {
///     if let f: ()->Int = **x.getCValue** {
///       return f()
///     }
///     return nil
///   }
///
///   // A more idiomatic implementation using "optional chaining"
///   func getCValue2(x: AnyObject) -> Int? {
///     return **x.getCValue?()**
///   }
///
///   // An implementation that assumes the required method is present
///   func getCValue3(x: AnyObject) -> **Int** {
///     return **x.getCValue()** // x.getCValue is implicitly unwrapped.
///   }
///
/// See also: `AnyClass`
@objc public protocol AnyObject {}

/// The protocol to which all class types implicitly conform.
///
/// When used as a concrete type, all known `@objc` `class` methods and
/// properties are available, as implicitly-unwrapped-optional methods
/// and properties respectively, on each instance of `AnyClass`. For
/// example:
///
/// .. parsed-literal:
///
///   class C {
///     @objc class var cValue: Int { return 42 }
///   }
///
///   // If x has an @objc cValue: Int, return its value.  
///   // Otherwise, return nil.
///   func getCValue(x: AnyClass) -> Int? {
///     return **x.cValue**
///   }
///
/// See also: `AnyObject`
public typealias AnyClass = AnyObject.Type

public func === (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
  switch (lhs, rhs) {
  case (.Some(let l), .Some(let r)):
    return Bool(Builtin.cmp_eq_RawPointer(
        Builtin.bridgeToRawPointer(Builtin.castToNativeObject(l)),
        Builtin.bridgeToRawPointer(Builtin.castToNativeObject(r))
      ))
  case (.None, .None):
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
  /// `x != y`
  func == (lhs: Self, rhs: Self) -> Bool
}

public func != <T : Equatable>(lhs: T, rhs: T) -> Bool {
  return !(lhs == rhs)
}

//
// Comparable
//

/// This protocol is an implementation detail of `Comparable`; do
/// not use it directly.
///
/// Its requirements are inherited by `Comparable` and thus must
/// be satisfied by types conforming to that protocol.
public protocol _Comparable {
  /// A `strict total order
  /// <http://en.wikipedia.org/wiki/Total_order#Strict_total_order>`_
  /// over instances of `Self`
  func <(lhs: Self, rhs: Self) -> Bool
}

public func > <T : _Comparable>(lhs: T, rhs: T) -> Bool {
  return rhs < lhs
}
public func <= <T : _Comparable>(lhs: T, rhs: T) -> Bool {
  return !(rhs < lhs)
}
public func >= <T : _Comparable>(lhs: T, rhs: T) -> Bool {
  return !(lhs < rhs)
}

/// Instances of conforming types can be compared using relational
/// operators, which define a `strict total order
/// <http://en.wikipedia.org/wiki/Total_order#Strict_total_order>`_.
///
/// A type conforming to `Comparable` need only supply the `<` and
/// `==` operators; default implementations of `<=`, `>`, `>=`, and
/// `!=` are supplied by the standard library::
///
///   struct Singular : Comparable {}
///   func ==(x: Singular, y: Singular) -> Bool { return true }
///   func <(x: Singular, y: Singular) -> Bool { return false }
///
/// **Axioms**, in addition to those of `Equatable`:
///
/// - `x == y` implies `x <= y`, `x >= y`, `!(x < y)`, and `!(x > y)`
/// - `x < y` implies `x <= y` and `y > x`
/// - `x > y` implies `x >= y` and `y < x`
/// - `x <= y` implies `y >= x`
/// - `x >= y` implies `y <= x`
public protocol Comparable : _Comparable, Equatable {
  func <=(lhs: Self, rhs: Self) -> Bool
  func >=(lhs: Self, rhs: Self) -> Bool
  func >(lhs: Self, rhs: Self) -> Bool
}

///
public protocol BitwiseOperationsType {
  func & (_: Self, _: Self) -> Self
  func |(_: Self, _: Self) -> Self
  func ^(_: Self, _: Self) -> Self
  prefix func ~(_: Self) -> Self

  /// The identity value for "|" and "^", and the fixed point for "&".
  ///
  /// ::
  ///
  ///   x | allZeros == x
  ///   x ^ allZeros == x
  ///   x & allZeros == allZeros
  ///   x & ~allZeros == x
  ///
  class var allZeros: Self { get }
}

public func |= <T: BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs | rhs
}

public func &= <T: BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs & rhs
}

public func ^= <T: BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs ^ rhs
}

public protocol Hashable : Equatable {
  /// Returns the hash value.  The hash value is not guaranteed to be stable
  /// across different invocations of the same program.  Do not persist the hash
  /// value across program runs.
  ///
  /// The value of `hashValue` property must be consistent with the equality
  /// comparison: if two values compare equal, they must have equal hash
  /// values.
  var hashValue: Int { get }
}

// The opposite of a GeneratorType (like an Output Iterator)
public protocol SinkType {
  typealias Element
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
infix operator  &/ { associativity left precedence 150 }
infix operator   % { associativity left precedence 150 }
infix operator  &% { associativity left precedence 150 }
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
infix operator ?? { associativity right precedence 110 }


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

