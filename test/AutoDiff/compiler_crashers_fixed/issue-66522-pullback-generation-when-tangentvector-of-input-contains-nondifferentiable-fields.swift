// RUN: %target-swift-frontend -emit-sil -O %s

import _Differentiation

// Issue #66522:
// Pullback generation for a function mapping a differentiable input type to 
// one of its differentiable fields fails when the input's tangent vector contains
// non-differentiable fields.                          
public struct P<Value>: Differentiable
where
    Value:  Differentiable,
    Value.TangentVector == Value,
    Value: AdditiveArithmetic {
  // `P` is its own `TangentVector`
  public typealias TangentVector = Self
  
  // Non-differentiable field in `P`'s `TangentVector`.
  public let name: String = ""
  var value: Value
}

extension P: Equatable, AdditiveArithmetic
where Value: AdditiveArithmetic {
    public static var zero: Self {fatalError()}
    public static func + (lhs: Self, rhs: Self) -> Self {fatalError()}
    public static func - (lhs: Self, rhs: Self) -> Self {fatalError()}
}

@differentiable(reverse)
internal func testFunction(data: P<Double>) -> Double {
    data.value
}
