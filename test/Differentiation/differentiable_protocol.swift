// RUN: %target-typecheck-verify-swift
// REQUIRES: differentiable_programming

import _Differentiation

// Test conformances.

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

// Test conformances for standard library types.

extension Float: Differentiable {
  public typealias TangentVector = Self
}
