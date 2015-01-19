// RUN: %target-parse-verify-swift -parse-stdlib -module-name Swift

// <rdar://problem/15593704>
struct X {
  var _maxLoadFactorInverse = 1.0 // expected-error{{expression does not conform to type 'FloatLiteralConvertible'}}
}

typealias MaxBuiltinFloatType = Builtin.FPIEEE64

protocol _BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

protocol FloatLiteralConvertible {
  typealias FloatLiteralType : _BuiltinFloatLiteralConvertible
  static func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}
