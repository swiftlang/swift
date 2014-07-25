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
public typealias _MaxBuiltinFloatType = Builtin.FPIEEE64

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

public typealias Any = protocol<>
@objc public protocol AnyObject {}

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

/// Types implementing the `Equatable` protocol can be compared for value
/// equality using operators `==` and `!=`.
///
/// When adopting `Equatable`, only the `==` operator is required to be
/// implemented.  The standard library provides an implementation for `!=`.
///
/// The `==` operator must define an equivalence relation.
public protocol Equatable {
  func == (lhs: Self, rhs: Self) -> Bool
}

public func != <T : Equatable>(lhs: T, rhs: T) -> Bool {
  return !(lhs == rhs)
}

//
// Comparable
//
public protocol _Comparable {
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

/// Types implementing the `Comparable` protocol can be compared using
/// relational operators `<`, `<=`, `>=`, `>`.
///
/// When adopting `Comparable`, only the `<` operator is required to be
/// implemented.  The standard library provides implementations for `<=`,
/// `>=`, `>`.
///
/// The `<` operator must define a total order.
public protocol Comparable : _Comparable, Equatable {
  func <=(lhs: Self, rhs: Self) -> Bool
  func >=(lhs: Self, rhs: Self) -> Bool
  func >(lhs: Self, rhs: Self) -> Bool
}

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

/*
// FIXME: These should be enabled, but <rdar://problem/17815767> 
public func |= <T: BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs | rhs
}

public func &= <T: BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs | rhs
}

public func ^= <T: BitwiseOperationsType>(inout lhs: T, rhs: T) {
  lhs = lhs | rhs
}
*/

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

public func == <T: _RawOptionSetType>(a: T, b: T) -> Bool {
  return a.toRaw() == b.toRaw()
}

/* FIXME: These should be default implementations of the BitwiseOperationsType
   conformance for RawOptionSetType. */
public func & <T: _RawOptionSetType>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() & b.toRaw())
}
public func | <T: _RawOptionSetType>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() | b.toRaw())
}
public func ^ <T: _RawOptionSetType>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() ^ b.toRaw())
}
public prefix func ~ <T: _RawOptionSetType>(a: T) -> T {
  return T.fromMask(~a.toRaw())
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

infix operator ++ { associativity left precedence 140 }
infix operator -- { associativity left precedence 140 }

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

//===--- Renamed protocols ------------------------------------------------===//
@availability(*,unavailable,renamed="ArrayBoundType")
public typealias ArrayBound = ArrayBoundType
@availability(*,unavailable,renamed="BidirectionalIndexType")
public typealias BidirectionalIndex = BidirectionalIndexType
@availability(*,unavailable,renamed="BitwiseOperationsType")
public typealias BitwiseOperations = BitwiseOperationsType
@availability(*,unavailable,renamed="CVarArgType")
public typealias CVarArg = CVarArgType
@availability(*,unavailable,renamed="CollectionType")
public typealias Collection = CollectionType
@availability(
  *,unavailable,renamed="ExtensibleCollectionType")
public typealias ExtensibleCollection = ExtensibleCollectionType
@availability(*,unavailable,renamed="FloatingPointType")
public typealias FloatingPointNumber = FloatingPointType
@availability(*,unavailable,renamed="ForwardIndexType")
public typealias ForwardIndex = ForwardIndexType
@availability(*,unavailable,renamed="GeneratorType")
public typealias Generator = GeneratorType
@availability(*,unavailable,renamed="IntegerType")
public typealias Integer = IntegerType
@availability(
  *,unavailable,renamed="IntegerArithmeticType")
public typealias IntegerArithmetic = IntegerArithmeticType
@availability(*,unavailable,renamed="BooleanType")
public typealias LogicValue = BooleanType
@availability(*,unavailable,renamed="MirrorType")
public typealias Mirror = MirrorType
@availability(
  *,unavailable,renamed="MutableCollectionType")
public typealias MutableCollection = MutableCollectionType
@availability(*,unavailable,renamed="OutputStreamType")
public typealias OutputStream = OutputStreamType
@availability(
  *,unavailable,renamed="RandomAccessIndexType")
public typealias RandomAccessIndex = RandomAccessIndexType
@availability(*,unavailable,renamed="RawOptionSetType")
public typealias RawOptionSet = RawOptionSetType
@availability(*,unavailable,renamed="SequenceType")
public typealias Sequence = SequenceType
@availability(*,unavailable,renamed="SignedIntegerType")
public typealias SignedInteger = SignedIntegerType
@availability(*,unavailable,renamed="SignedNumberType")
public typealias SignedNumber = SignedNumberType
@availability(*,unavailable,renamed="SinkType")
public typealias Sink = SinkType
@availability(*,unavailable,renamed="UnicodeCodecType")
public typealias UnicodeCodec = UnicodeCodecType
@availability(*,unavailable,renamed="UnsignedIntegerType")
public typealias UnsignedInteger = UnsignedIntegerType
@availability(*,unavailable,renamed="_BidirectionalIndexType")
public typealias _BidirectionalIndex = _BidirectionalIndexType
@availability(*,unavailable,renamed="_BridgedToObjectiveCType")
public typealias _BridgedToObjectiveC = _BridgedToObjectiveCType
@availability(*,unavailable,renamed="_CocoaStringType")
public typealias _CocoaString = _CocoaStringType
@availability(*,unavailable,renamed="_CollectionType")
public typealias _Collection = _CollectionType
@availability(*,unavailable,
  renamed="_ConditionallyBridgedToObjectiveCType")
public typealias _ConditionallyBridgedToObjectiveC
  = _ConditionallyBridgedToObjectiveCType
@availability(
  *,unavailable,renamed="_ExtensibleCollectionType")
public typealias _ExtensibleCollection = _ExtensibleCollectionType
@availability(*,unavailable,renamed="_ForwardIndexType")
public typealias _ForwardIndex = _ForwardIndexType
@availability(*,unavailable,renamed="_IntegerType")
public typealias _Integer = _IntegerType
@availability(
  *,unavailable,renamed="_IntegerArithmeticType")
public typealias _IntegerArithmetic = _IntegerArithmeticType
@availability(
  *,unavailable,renamed="_PrintableNSObjectType")
public typealias _PrintableNSObject = _PrintableNSObjectType
@availability(
  *,unavailable,renamed="_RandomAccessIndexType")
public typealias _RandomAccessIndex = _RandomAccessIndexType
@availability(*,unavailable,renamed="_RawOptionSetType")
public typealias _RawOptionSet = _RawOptionSetType
@availability(*,unavailable,renamed="_SequenceType")
public typealias _Sequence = _SequenceType
@availability(*,unavailable,renamed="_Sequence_Type")
public typealias _Sequence_ = _Sequence_Type
@availability(*,unavailable,renamed="_SignedIntegerType")
public typealias _SignedInteger = _SignedIntegerType
@availability(*,unavailable,renamed="_SignedNumberType")
public typealias _SignedNumber = _SignedNumberType
@availability(*,unavailable,renamed="_UnsignedIntegerType")
public typealias _UnsignedInteger = _UnsignedIntegerType
