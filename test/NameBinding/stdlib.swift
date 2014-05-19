// RUN: %swift -parse-stdlib -module-name=Swift %s -parse -verify 

// <rdar://problem/15593704>
struct X {
  var _maxLoadFactorInverse = 1.0 // expected-error{{cannot convert the expression's type '$T0' to type 'FloatLiteralConvertible'}}
}

typealias MaxBuiltinFloatType = Builtin.FPIEEE64

protocol _BuiltinFloatLiteralConvertible {
  class func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

protocol FloatLiteralConvertible {
  typealias FloatLiteralType : _BuiltinFloatLiteralConvertible
  class func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}
