// RUN: %target-typecheck-verify-swift

protocol P {
  static func << (lhs: Self, rhs: Self) -> Self
  static func >> (lhs: Self, rhs: Self) -> Self
  static func <<= (lhs: inout Self, rhs: Self)
  static func >>= (lhs: inout Self, rhs: Self)
}
