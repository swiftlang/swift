// RUN: %swift -parse-stdlib -module-name=swift %s -parse -verify 

// <rdar://problem/15593704>
struct X {
  var _maxLoadFactorInverse = 1.0 // expected-error{{expression does not type-check}}
}

typealias MaxBuiltinFloatType = Builtin.FPIEEE64

protocol BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

protocol FloatLiteralConvertible {
  typealias FloatLiteralType : BuiltinFloatLiteralConvertible
  static func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}
