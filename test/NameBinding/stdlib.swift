// RUN: %target-parse-verify-swift -parse-stdlib -module-name Swift

enum Optional<T> {
  case none
  case some(T)
}

// <rdar://problem/15593704>
struct X {
  // This is in parse-stdlib mode with no default literal type.
  var _maxLoadFactorInverse = 1.0 // expected-error{{standard library error: _MaxBuiltinFloatType is not properly defined}}
}

typealias MaxBuiltinFloatType = Builtin.FPIEEE64

protocol _BuiltinFloatLiteralConvertible {
  static func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

protocol FloatLiteralConvertible {
  associatedtype FloatLiteralType : _BuiltinFloatLiteralConvertible
  static func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}
