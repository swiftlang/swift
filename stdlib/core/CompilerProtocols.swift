//===----------------------------------------------------------------------===//
// Intrinsic protocols shared with the compiler
//===----------------------------------------------------------------------===//

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

protocol Generator {
  typealias Element
  func next() -> Element?
}

protocol Sequence {
  typealias GeneratorType : Generator
  func enumerate() -> GeneratorType
}

protocol RawRepresentable {
  typealias RawType
  static func fromRaw(_: RawType) -> Self?
  func toRaw() -> RawType
}

// TODO: This is an incomplete implementation of our option sets vision.
protocol RawOptionSet : RawRepresentable, LogicValue
                        /*FIXME: , BitwiseOperations*/ {
  typealias RawType : BitwiseOperations

  // A non-failable version of RawRepresentable.fromRaw.
  static func fromMask(_: RawType) -> Self

  // FIXME: Disabled pending <rdar://problem/14011860> (Default
  // implementations in protocols)
  // The Clang importer synthesizes these for imported NS_OPTIONS.

  /* static func fromRaw(raw: RawType) -> Self? { return fromMask(raw) } */

  /* func getLogicValue() -> Bool { return toRaw() != .allZeros() } */
}

protocol BuiltinIntegerLiteralConvertible {
  static func _convertFromBuiltinIntegerLiteral(
                value: MaxBuiltinIntegerType) -> Self
}

protocol IntegerLiteralConvertible {
  typealias IntegerLiteralType : BuiltinIntegerLiteralConvertible
  static func convertFromIntegerLiteral(value: IntegerLiteralType) -> Self
}

protocol BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

protocol FloatLiteralConvertible {
  typealias FloatLiteralType : BuiltinFloatLiteralConvertible
  static func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}

protocol BuiltinCharacterLiteralConvertible {
  static func _convertFromBuiltinCharacterLiteral(value: Builtin.Int21) -> Self
}

protocol CharacterLiteralConvertible {
  typealias CharacterLiteralType : BuiltinCharacterLiteralConvertible
  static func convertFromCharacterLiteral(value: CharacterLiteralType) -> Self
}

protocol BuiltinStringLiteralConvertible {
  static func _convertFromBuiltinStringLiteral(value: Builtin.RawPointer,
                                               byteSize: Builtin.Int64,
                                               isASCII: Builtin.Int1) -> Self
}

protocol StringLiteralConvertible {
  typealias StringLiteralType : BuiltinStringLiteralConvertible
  static func convertFromStringLiteral(value: StringLiteralType) -> Self
}

protocol ArrayLiteralConvertible {
  typealias Element
  static func convertFromArrayLiteral(elements: Element...) -> Self
}

protocol DictionaryLiteralConvertible {
  typealias Key
  typealias Value
  static func convertFromDictionaryLiteral(elements: (Key, Value)...) -> Self
}

protocol StringInterpolationConvertible {
  static func convertFromStringInterpolation(strings: Self...) -> Self
}

//===----------------------------------------------------------------------===//
// REPL protocols
//===----------------------------------------------------------------------===//

// FIXME: This should be intrinsically available for any metatype--need a
// metatype root type
protocol ClassNameable {
  static func className() -> String
}

protocol ReplPrintable {
  func replPrint()
}
