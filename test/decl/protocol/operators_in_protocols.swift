// RUN: %target-parse-verify-swift

protocol P {
  func << (lhs: Self, rhs: Self) -> Self
  func >> (lhs: Self, rhs: Self) -> Self
  func <<= (lhs: inout Self, rhs: Self)
  func >>= (lhs: inout Self, rhs: Self)
}
