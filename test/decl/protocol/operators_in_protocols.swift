// RUN: %swift %s -parse -verify

protocol P {
  func << (lhs: Self, rhs: Self) -> Self
  func >> (lhs: Self, rhs: Self) -> Self
  func <<= (inout lhs: Self, rhs: Self)
  func >>= (inout lhs: Self, rhs: Self)
}
