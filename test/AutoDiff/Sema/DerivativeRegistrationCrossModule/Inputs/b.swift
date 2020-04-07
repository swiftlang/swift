import _Differentiation
import a

extension Struct: Differentiable {
  public struct TangentVector: Differentiable & AdditiveArithmetic {}
  public mutating func move(along _: TangentVector) {}

  @usableFromInline
  @derivative(of: method, wrt: x)
  func vjpMethod(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { $0 })
  }

  @usableFromInline
  @derivative(of: +)
  static func vjpAdd(_ lhs: Self, rhs: Self) -> (
    value: Self, pullback: (TangentVector) -> (TangentVector, TangentVector)
  ) {
    (lhs + rhs, { v in (v, v) })
  }
}
