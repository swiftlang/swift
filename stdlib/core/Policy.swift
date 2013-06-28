// RUN: %swift %s -verify -parse-as-library

//===----------------------------------------------------------------------===//
// Swift Standard Prolog Library.
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Standardized aliases
//===----------------------------------------------------------------------===//
typealias Void   = ()
typealias Int    = Int64
typealias UInt   = UInt64
typealias Float  = Float32
typealias Double = Float64

// FIXME/TBD: Consider adding "int", "double", etc as aliases for Int/Double.
// They violate the naming convention but lower the barrier to entry.

//===----------------------------------------------------------------------===//
// Default types for unconstrained number literals
//===----------------------------------------------------------------------===//
typealias IntegerLiteralType = Int
typealias FloatLiteralType = Double
typealias CharacterLiteralType = Char

//===----------------------------------------------------------------------===//
// Default types for unconstrained number literals
//===----------------------------------------------------------------------===//
typealias MaxBuiltinIntegerType = Builtin.Int128
typealias MaxBuiltinFloatType = Builtin.FPIEEE64

//===----------------------------------------------------------------------===//
// Standard "typeof" function
//===----------------------------------------------------------------------===//

func typeof<T>(x:T) -> T.metatype { return Builtin.typeof(x) }

//===----------------------------------------------------------------------===//
// Standard protocols
//===----------------------------------------------------------------------===//

typealias Any = protocol<>

// FIXME: These protocols are a workaround for
// <rdar://problem/13251236> remapping bound function type not
// implemented yet
protocol Callable {
  typealias Arguments
  typealias Result
  func __call__(args: Arguments) -> Result
}

protocol Predicate : Callable {
  typealias Result : LogicValue
}

func apply<F: Callable>(f: F, args: F.Arguments) -> F.Result {
  return f.__call__(args)
}

//
// Identifiable
//
protocol Identifiable {
  func __eq__(rhs: This) -> Bool
}

func ===<T: Identifiable>(lhs: T, rhs: T) -> Bool {
  return lhs.__eq__(rhs)
}
func !==<T: Identifiable>(lhs: T, rhs: T) -> Bool {
  return !lhs.__eq__(rhs)
}

//
// Equatable
//
protocol Equatable {
  func __equal__(rhs: This) -> Bool
}

func == <T: Equatable>(lhs: T, rhs: T) -> Bool {
  return lhs.__equal__(rhs)
}
func != <T: Equatable>(lhs: T, rhs: T) -> Bool {
  return !lhs.__equal__(rhs)
}

//
// Comparable
//
protocol Comparable : Equatable {
  func __less__(rhs : This) -> Bool
}
func < <T: Comparable>(lhs: T, rhs: T) -> Bool {
  return lhs.__less__(rhs)
}
func > <T: Comparable>(lhs: T, rhs: T) -> Bool {
  return rhs.__less__(lhs)
}
func <= <T: Comparable>(lhs: T, rhs: T) -> Bool {
  return !rhs.__less__(lhs)
}
func >= <T: Comparable>(lhs: T, rhs: T) -> Bool {
  return !lhs.__less__(rhs)
}

protocol ForwardIndex : Equatable {
  func succ() -> This
}

func [prefix, assignment] 
  ++ <T: ForwardIndex> (x: [byref] T) 
-> T {
  x = x.succ()
  return x
}


func [postfix, assignment] 
  ++ <T: ForwardIndex> (x: [byref] T)
-> T {
  var ret = x
  x = x.succ()
  return ret
}

protocol BidirectionalIndex : ForwardIndex {
  func pred() -> This
}

func [prefix, assignment] 
  -- <T: BidirectionalIndex> (x: [byref] T)
-> T {
  x = x.pred()
  return x
}


func [postfix, assignment] 
 -- <T: BidirectionalIndex> (x: [byref] T) 
-> T {
  var ret = x
  x = x.pred()
  return ret
}

protocol RandomAccessIndex : BidirectionalIndex, Comparable {
  typealias DistanceType
  func __sub__(rhs: This) -> DistanceType
  func __sub__(rhs: DistanceType) -> This
  func __add__(offset: DistanceType) -> This
  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  func __less__(rhs: This) -> Bool /* {
      return (lhs.__sub__(rhs)).isNegative()

  } */
}

func [infix] - <T: RandomAccessIndex>(x: T, y: T) -> T.DistanceType {
  return x.__sub__(y)
}

func [infix] - <T: RandomAccessIndex>(x: T, y: T.DistanceType) -> T {
  return x.__sub__(y)
}

protocol SignedNumber {
  func __negate__() -> This
  func isNegative() -> Bool
}

func [prefix] - <T: SignedNumber> (x: T) -> T {
  return x.__negate__()
}

func [infix, assignment] += 
  <T: RandomAccessIndex> (lhs: [byref] T, rhs: T.DistanceType) {
  lhs = lhs.__add__(rhs)
}

func [infix, assignment] -= <
  T: RandomAccessIndex requires T.DistanceType: SignedNumber
> (lhs: [byref] T, rhs: T.DistanceType) {
  lhs = lhs.__add__(-rhs)
}

func [infix] + 
  <T: RandomAccessIndex> (lhs: T, rhs: T.DistanceType) 
-> T {
  return lhs.__add__(rhs)
}

func [infix] + 
  <T: RandomAccessIndex> (lhs: T.DistanceType, rhs: T) 
-> T {
  return rhs.__add__(lhs)
}

func [infix] -
  <T: RandomAccessIndex requires T.DistanceType: SignedNumber> (
  lhs: T, rhs: T.DistanceType) 
-> T {
  return lhs.__add__(-rhs)
}

protocol Hashable : Equatable {
  func hashValue() -> Int
}

/// \brief Protocol describing types that can be used as array bounds.
///
/// Types that conform to the \c ArrayBound protocol can be used as array bounds 
/// by providing an operation (\c getArrayBoundValue) that produces an integral 
/// value.
protocol ArrayBound {
  typealias ArrayBoundType
  func getArrayBoundValue() -> ArrayBoundType
}

/// \brief Protocol describing types that can be used as logical values within
/// a condition.
///
/// Types that conform to the \c LogicValue protocol can be used as
/// condition in various control statements (\c if, \c while, C-style
/// \c for) as well as other logical value contexts (e.g., case
/// statement guards).
protocol LogicValue {
  func getLogicValue() -> Bool
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol Enumerator {
  typealias Element
  func isEmpty() -> Bool
  func next() -> Element
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol Enumerable {
  typealias EnumeratorType : Enumerator
  func getEnumeratorType() -> EnumeratorType
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol BuiltinIntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(
                value : MaxBuiltinIntegerType) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol IntegerLiteralConvertible {
  typealias IntegerLiteralType : BuiltinIntegerLiteralConvertible
  static func convertFromIntegerLiteral(value : IntegerLiteralType) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(
                value : MaxBuiltinFloatType) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol FloatLiteralConvertible {
  typealias FloatLiteralType : BuiltinFloatLiteralConvertible
  static func convertFromFloatLiteral(value : FloatLiteralType) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol BuiltinCharacterLiteralConvertible {
  static func _convertFromBuiltinCharacterLiteral(value : Builtin.Int32) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol CharacterLiteralConvertible {
  typealias CharacterLiteralType : BuiltinCharacterLiteralConvertible
  static func convertFromCharacterLiteral(value : CharacterLiteralType) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol BuiltinStringLiteralConvertible {
  static func _convertFromBuiltinStringLiteral(value : Builtin.RawPointer,
                                               byteSize : Builtin.Int64,
                                               isASCII: Builtin.Int1) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol StringLiteralConvertible {
  typealias StringLiteralType : BuiltinStringLiteralConvertible
  static func convertFromStringLiteral(value : StringLiteralType) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol ArrayLiteralConvertible {
  typealias Element
  static func convertFromArrayLiteral(elements : Element...) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol DictionaryLiteralConvertible {
  typealias Key
  typealias Value
  static func convertFromDictionaryLiteral(elements:(Key, Value)...) -> This
}

// NOTE: the compiler has builtin knowledge of this protocol
protocol StringInterpolationConvertible {
  static func convertFromStringInterpolation(strings : This...) -> This
}

// NOTE: the repl has builtin knowledge of this protocol
// FIXME: This should be intrinsically available for any metatype--need a
// metatype root type
protocol ClassNameable {
  static func className() -> String
}

//===----------------------------------------------------------------------===//
// Standard operators
//===----------------------------------------------------------------------===//

operator postfix ++ {}
operator postfix -- {}

operator prefix ++ {}
operator prefix -- {}
operator prefix ! {}
operator prefix ~ {}
operator prefix + {}
operator prefix - {}

operator infix * {
  associativity left
  precedence 200
}
operator infix / {
  associativity left
  precedence 200
}
operator infix % {
  associativity left
  precedence 200
}

operator infix + {
  associativity left
  precedence 190
}
operator infix - {
  associativity left
  precedence 190
}

operator infix << {
  associativity none
  precedence 180
}
operator infix >> {
  associativity none
  precedence 180
}

operator infix .. {
  associativity none
  precedence 175
}

operator infix < {
  associativity none
  precedence 170
}
operator infix <= {
  associativity none
  precedence 170
}
operator infix > {
  associativity none
  precedence 170
}
operator infix >= {
  associativity none
  precedence 170
}

operator infix == {
  associativity none
  precedence 160
}
operator infix != {
  associativity none
  precedence 160
}
operator infix === {
  associativity none
  precedence 160
}
operator infix !== {
  associativity none
  precedence 160
}

operator infix & {
  associativity left
  precedence 150
}

operator infix ^ {
  associativity left
  precedence 140
}

operator infix | {
  associativity left
  precedence 130
}

operator infix && {
  associativity left
  precedence 120
}

operator infix || {
  associativity left
  precedence 110
}

// User-defined ternary operators are not supported. The ? : operator is
// hardcoded as if it had the following attributes:
// operator ternary ? : {
//   associativity right
//   precedence 100
// }

operator infix *= {
  associativity right
  precedence 90
}
operator infix /= {
  associativity right
  precedence 90
}
operator infix %= {
  associativity right
  precedence 90
}
operator infix += {
  associativity right
  precedence 90
}
operator infix -= {
  associativity right
  precedence 90
}
operator infix <<= {
  associativity right
  precedence 90
}
operator infix >>= {
  associativity right
  precedence 90
}
operator infix &= {
  associativity right
  precedence 90
}
operator infix ^= {
  associativity right
  precedence 90
}
operator infix |= {
  associativity right
  precedence 90
}
operator infix &&= {
  associativity right
  precedence 90
}
operator infix ||= {
  associativity right
  precedence 90
}

