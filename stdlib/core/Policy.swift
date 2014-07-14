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
public typealias MaxBuiltinIntegerType = Builtin.Int2048
public typealias MaxBuiltinFloatType = Builtin.FPIEEE64

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

public protocol Hashable : Equatable {
  /// Returns the hash value.  The hash value is not guaranteed to be stable
  /// across different invocations of the same program.  Do not persist the hash
  /// value across program runs.
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
public func & <T: RawOptionSetType>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() & b.toRaw())
}
public func | <T: RawOptionSetType>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() | b.toRaw())
}
public func ^ <T: RawOptionSetType>(a: T, b: T) -> T {
  return T.fromMask(a.toRaw() ^ b.toRaw())
}
public prefix func ~ <T: RawOptionSetType>(a: T) -> T {
  return T.fromMask(~a.toRaw())
}

//===----------------------------------------------------------------------===//
// Standard pattern matching forms
//===----------------------------------------------------------------------===//

// Equatable types can be matched in patterns by value equality.
@transparent public
func ~= <T : Equatable> (a: T, b: T) -> Bool {
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


// User-defined ternary operators are not supported. The ? : operator is
// hardcoded as if it had the following attributes:
// operator ternary ? : { associativity right precedence 100 }

// User-defined assignment operators are not supported. The = operator is
// hardcoded as if it had the following attributes:
// infix operator = { associativity right precedence 90 }

// Compound

infix operator  *= { associativity right precedence 90 }
infix operator  /= { associativity right precedence 90 }
infix operator  %= { associativity right precedence 90 }
infix operator  += { associativity right precedence 90 }
infix operator  -= { associativity right precedence 90 }
infix operator <<= { associativity right precedence 90 }
infix operator >>= { associativity right precedence 90 }
infix operator  &= { associativity right precedence 90 }
infix operator  ^= { associativity right precedence 90 }
infix operator  |= { associativity right precedence 90 }

// Workaround for <rdar://problem/14011860> SubTLF: Default
// implementations in protocols.  Library authors should ensure
// that this operator never needs to be seen by end-users.  See
// test/Prototypes/GenericDispatch.swift for a fully documented
// example of how this operator is used, and how its use can be hidden
// from users.
infix operator ~> { associativity left precedence 255 }

//===--- Renamed protocols ------------------------------------------------===//
@availability(*,unavailable,message="it has been renamed 'ArrayBoundType'")
public typealias ArrayBound = ArrayBoundType
@availability(
  *,unavailable,message="it has been renamed 'BidirectionalIndexType'")
public typealias BidirectionalIndex = BidirectionalIndexType
@availability(
  *,unavailable,message="it has been renamed 'BitwiseOperationsType'")
public typealias BitwiseOperations = BitwiseOperationsType
@availability(*,unavailable,message="it has been renamed 'CVarArgType'")
public typealias CVarArg = CVarArgType
@availability(*,unavailable,message="it has been renamed 'CollectionType'")
public typealias Collection = CollectionType
@availability(
  *,unavailable,message="it has been renamed 'ExtensibleCollectionType'")
public typealias ExtensibleCollection = ExtensibleCollectionType
@availability(*,unavailable,message="it has been renamed 'FloatingPointType'")
public typealias FloatingPointNumber = FloatingPointType
@availability(*,unavailable,message="it has been renamed 'ForwardIndexType'")
public typealias ForwardIndex = ForwardIndexType
@availability(*,unavailable,message="it has been renamed 'GeneratorType'")
public typealias Generator = GeneratorType
@availability(*,unavailable,message="it has been renamed 'IntegerType'")
public typealias Integer = IntegerType
@availability(
  *,unavailable,message="it has been renamed 'IntegerArithmeticType'")
public typealias IntegerArithmetic = IntegerArithmeticType
@availability(*,unavailable,message="it has been renamed 'BooleanType'")
public typealias LogicValue = BooleanType
@availability(*,unavailable,message="it has been renamed 'MirrorType'")
public typealias Mirror = MirrorType
@availability(
  *,unavailable,message="it has been renamed 'MutableCollectionType'")
public typealias MutableCollection = MutableCollectionType
@availability(*,unavailable,message="it has been renamed 'OutputStreamType'")
public typealias OutputStream = OutputStreamType
@availability(
  *,unavailable,message="it has been renamed 'RandomAccessIndexType'")
public typealias RandomAccessIndex = RandomAccessIndexType
@availability(*,unavailable,message="it has been renamed 'RawOptionSetType'")
public typealias RawOptionSet = RawOptionSetType
@availability(*,unavailable,message="it has been renamed 'SequenceType'")
public typealias Sequence = SequenceType
@availability(*,unavailable,message="it has been renamed 'SignedIntegerType'")
public typealias SignedInteger = SignedIntegerType
@availability(*,unavailable,message="it has been renamed 'SignedNumberType'")
public typealias SignedNumber = SignedNumberType
@availability(*,unavailable,message="it has been renamed 'SinkType'")
public typealias Sink = SinkType
@availability(*,unavailable,message="it has been renamed 'UnicodeCodecType'")
public typealias UnicodeCodec = UnicodeCodecType
@availability(*,unavailable,message="it has been renamed 'UnsignedIntegerType'")
public typealias UnsignedInteger = UnsignedIntegerType
@availability(
  *,unavailable,message="it has been renamed '_BidirectionalIndexType'")
public typealias _BidirectionalIndex = _BidirectionalIndexType
@availability(
  *,unavailable,message="it has been renamed '_BridgedToObjectiveCType'")
public typealias _BridgedToObjectiveC = _BridgedToObjectiveCType
@availability(*,unavailable,message="it has been renamed '_CocoaStringType'")
public typealias _CocoaString = _CocoaStringType
@availability(*,unavailable,message="it has been renamed '_CollectionType'")
public typealias _Collection = _CollectionType
@availability(
  *,unavailable,
  message="it has been renamed '_ConditionallyBridgedToObjectiveCType'")
public typealias _ConditionallyBridgedToObjectiveC
  = _ConditionallyBridgedToObjectiveCType
@availability(
  *,unavailable,message="it has been renamed '_ExtensibleCollectionType'")
public typealias _ExtensibleCollection = _ExtensibleCollectionType
@availability(*,unavailable,message="it has been renamed '_ForwardIndexType'")
public typealias _ForwardIndex = _ForwardIndexType
@availability(*,unavailable,message="it has been renamed '_IntegerType'")
public typealias _Integer = _IntegerType
@availability(
  *,unavailable,message="it has been renamed '_IntegerArithmeticType'")
public typealias _IntegerArithmetic = _IntegerArithmeticType
@availability(
  *,unavailable,message="it has been renamed '_PrintableNSObjectType'")
public typealias _PrintableNSObject = _PrintableNSObjectType
@availability(
  *,unavailable,message="it has been renamed '_RandomAccessIndexType'")
public typealias _RandomAccessIndex = _RandomAccessIndexType
@availability(*,unavailable,message="it has been renamed '_RawOptionSetType'")
public typealias _RawOptionSet = _RawOptionSetType
@availability(*,unavailable,message="it has been renamed '_SequenceType'")
public typealias _Sequence = _SequenceType
@availability(*,unavailable,message="it has been renamed '_Sequence_Type'")
public typealias _Sequence_ = _Sequence_Type
@availability(*,unavailable,message="it has been renamed '_SignedIntegerType'")
public typealias _SignedInteger = _SignedIntegerType
@availability(*,unavailable,message="it has been renamed '_SignedNumberType'")
public typealias _SignedNumber = _SignedNumberType
@availability(
  *,unavailable,message="it has been renamed '_UnsignedIntegerType'")
public typealias _UnsignedInteger = _UnsignedIntegerType
