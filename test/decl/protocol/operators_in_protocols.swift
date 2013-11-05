// RUN: %swift %s -parse -verify

protocol P {
  def << (lhs: Self, rhs: Self) -> Self
  def >> (lhs: Self, rhs: Self) -> Self
  def <<= (lhs: @inout Self, rhs: Self)
  def >>= (lhs: @inout Self, rhs: Self)
}
