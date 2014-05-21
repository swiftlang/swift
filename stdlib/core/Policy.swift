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

import SwiftShims

//===----------------------------------------------------------------------===//
// Standardized aliases
//===----------------------------------------------------------------------===//
typealias Void = ()

//===----------------------------------------------------------------------===//
// Aliases for floating point types
//===----------------------------------------------------------------------===//
// FIXME: it should be the other way round, Float = Float32, Double = Float64,
// but the type checker loses sugar currently, and ends up displaying 'FloatXX'
// in diagnostics.
typealias Float32 = Float
typealias Float64 = Double

//===----------------------------------------------------------------------===//
// Default types for unconstrained literals
//===----------------------------------------------------------------------===//
typealias IntegerLiteralType = Int
typealias FloatLiteralType = Double
// typealias CharacterLiteralType = ?
typealias ExtendedGraphemeClusterType = String
typealias StringLiteralType = String

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
typealias MaxBuiltinIntegerType = Builtin.Int2048
typealias MaxBuiltinFloatType = Builtin.FPIEEE64

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

typealias Any = protocol<>
@class_protocol @objc
protocol AnyObject {}

typealias AnyClass = AnyObject.Type

func === (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
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

func !== (lhs: AnyObject?, rhs: AnyObject?) -> Bool {
  return !(lhs === rhs)
}

//
// Equatable
//
protocol Equatable {
  func == (lhs: Self, rhs: Self) -> Bool
}

func != <T : Equatable>(lhs: T, rhs: T) -> Bool {
  return !(lhs == rhs)
}

//
// Comparable
//
protocol _Comparable {
  func <(lhs: Self, rhs: Self) -> Bool
}

func > <T : _Comparable>(lhs: T, rhs: T) -> Bool {
  return rhs < lhs
}
func <= <T : _Comparable>(lhs: T, rhs: T) -> Bool {
  return !(rhs < lhs)
}
func >= <T : _Comparable>(lhs: T, rhs: T) -> Bool {
  return !(lhs < rhs)
}

protocol Comparable : _Comparable, Equatable {
  func <=(lhs: Self, rhs: Self) -> Bool
  func >=(lhs: Self, rhs: Self) -> Bool
  func >(lhs: Self, rhs: Self) -> Bool
}

protocol BitwiseOperations {
  func & (_: Self, _: Self) -> Self
  func |(_: Self, _: Self) -> Self
  func ^(_: Self, _: Self) -> Self
  @prefix func ~(_: Self) -> Self

  /// The identity value for "|" and "^", and the fixed point for "&".
  ///
  /// ::
  ///
  ///   x | allZeros() == x
  ///   x ^ allZeros() == x
  ///   x & allZeros() == allZeros()
  ///   x & ~allZeros() == x
  ///
  class var allZeros: Self { get }
}

protocol Hashable : Equatable {
  /// Returns the hash value.  The hash value is not guaranteed to be stable
  /// across different invocations of the same program.  Do not persist the hash
  /// value across program runs.
  var hashValue: Int { get }
}

// The opposite of a Generator (like an Output Iterator)
protocol Sink {
  typealias Element
  mutating func put(x: Element)
}

func == <T: _RawOptionSet>(a: T, b: T) -> Bool {
  return a.toRaw() == b.toRaw()
}

/* FIXME: These should be default implementations of the BitwiseOperations
   conformance for RawOptionSet. */
func & <T: RawOptionSet>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() & b.toRaw())
}
func | <T: RawOptionSet>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() | b.toRaw())
}
func ^ <T: RawOptionSet>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() ^ b.toRaw())
}
@prefix func ~ <T: RawOptionSet>(a: T) -> T {
  return T.fromMask(~a.toRaw())
}

//===----------------------------------------------------------------------===//
// Standard pattern matching forms
//===----------------------------------------------------------------------===//

// Equatable types can be matched in patterns by value equality.
@transparent @infix
func ~= <T : Equatable> (a: T, b: T) -> Bool {
  return a == b
}

//===----------------------------------------------------------------------===//
// Standard operators
//===----------------------------------------------------------------------===//

// Standard postfix operators.
operator postfix ++ {}
operator postfix -- {}

// Optional<T> unwrapping operator is built into the compiler as a part of
// postfix expression grammar.
//
// operator postfix ! {}

// Standard prefix operators.
operator prefix ++ {}
operator prefix -- {}
operator prefix ! {}
operator prefix ~ {}
operator prefix + {}
operator prefix - {}

// Standard infix operators.

// "Exponentiative"

operator infix << { associativity none precedence 160 }
operator infix >> { associativity none precedence 160 }

// "Multiplicative"

operator infix   * { associativity left precedence 150 }
operator infix  &* { associativity left precedence 150 }
operator infix   / { associativity left precedence 150 }
operator infix  &/ { associativity left precedence 150 }
operator infix   % { associativity left precedence 150 }
operator infix  &% { associativity left precedence 150 }
operator infix   & { associativity left precedence 150 }

// "Additive"

operator infix   + { associativity left precedence 140 }
operator infix  &+ { associativity left precedence 140 }
operator infix   - { associativity left precedence 140 }
operator infix  &- { associativity left precedence 140 }
operator infix   | { associativity left precedence 140 }
operator infix   ^ { associativity left precedence 140 }

// FIXME: is this the right precedence level for "..." ?
operator infix  ... { associativity none precedence 135 }
operator infix  ..  { associativity none precedence 135 }

// The cast operators 'as' and 'is' are hardcoded as if they had the
// following attributes:
// operator infix as { associativity none precedence 132 }

// "Comparative"

operator infix  <  { associativity none precedence 130 }
operator infix  <= { associativity none precedence 130 }
operator infix  >  { associativity none precedence 130 }
operator infix  >= { associativity none precedence 130 }
operator infix  == { associativity none precedence 130 }
operator infix  != { associativity none precedence 130 }
operator infix === { associativity none precedence 130 }
operator infix !== { associativity none precedence 130 }
// FIXME: ~= will be built into the compiler.
operator infix  ~= { associativity none precedence 130 }

// "Conjunctive"

operator infix && { associativity left precedence 120 }

// "Disjunctive"

operator infix || { associativity left precedence 110 }


// User-defined ternary operators are not supported. The ? : operator is
// hardcoded as if it had the following attributes:
// operator ternary ? : { associativity right precedence 100 }

// User-defined assignment operators are not supported. The = operator is
// hardcoded as if it had the following attributes:
// operator infix = { associativity right precedence 90 }

// Compound

operator infix  *= { associativity right precedence 90 }
operator infix  /= { associativity right precedence 90 }
operator infix  %= { associativity right precedence 90 }
operator infix  += { associativity right precedence 90 }
operator infix  -= { associativity right precedence 90 }
operator infix <<= { associativity right precedence 90 }
operator infix >>= { associativity right precedence 90 }
operator infix  &= { associativity right precedence 90 }
operator infix  ^= { associativity right precedence 90 }
operator infix  |= { associativity right precedence 90 }
operator infix &&= { associativity right precedence 90 }
operator infix ||= { associativity right precedence 90 }

// Workaround for <rdar://problem/14011860> SubTLF: Default
// implementations in protocols.  Library authors should ensure
// that this operator never needs to be seen by end-users.  See
// test/Prototypes/GenericDispatch.swift for a fully documented
// example of how this operator is used, and how its use can be hidden
// from users.
operator infix ~> { associativity left precedence 255 }
