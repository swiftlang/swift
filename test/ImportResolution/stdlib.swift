// RUN: %target-typecheck-verify-swift -parse-stdlib -module-name Swift

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

protocol _ExpressibleByBuiltinFloatLiteral {
  static func _convertFromBuiltinFloatLiteral(
                value: MaxBuiltinFloatType) -> Self
}

protocol ExpressibleByFloatLiteral {
  associatedtype FloatLiteralType : _ExpressibleByBuiltinFloatLiteral
  static func convertFromFloatLiteral(value: FloatLiteralType) -> Self
}
