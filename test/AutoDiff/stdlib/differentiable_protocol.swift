// RUN: %target-typecheck-verify-swift
// REQUIRES: differentiable_programming

import _Differentiation

// Test `Differentiable` protocol conformances.

struct FloatWrapper {
  var value: Float
}
extension FloatWrapper: AdditiveArithmetic {
  static var zero: Self {
    FloatWrapper(value: Float.zero)
  }
  static func + (lhs: Self, rhs: Self) -> Self {
    return FloatWrapper(value: lhs.value + rhs.value)
  }
  static func - (lhs: Self, rhs: Self) -> Self {
    return FloatWrapper(value: lhs.value + rhs.value)
  }
}
extension FloatWrapper: Differentiable {
  public typealias TangentVector = Self
}

struct Wrapper<T> {
  var value: T
}
extension Wrapper: Equatable where T: Equatable {}
extension Wrapper: AdditiveArithmetic where T: AdditiveArithmetic {
  static var zero: Self {
    Wrapper(value: T.zero)
  }
  static func + (lhs: Self, rhs: Self) -> Self {
    return Wrapper(value: lhs.value + rhs.value)
  }
  static func - (lhs: Self, rhs: Self) -> Self {
    return Wrapper(value: lhs.value + rhs.value)
  }
}
extension Wrapper: Differentiable where T: Differentiable {
  typealias TangentVector = Wrapper<T.TangentVector>
  mutating func move(along direction: TangentVector) {
    value.move(along: direction.value)
  }
}
