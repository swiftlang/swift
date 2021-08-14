import _Differentiation
import a

extension Struct: Differentiable {
  public struct TangentVector: Differentiable & AdditiveArithmetic {}
  public mutating func move(by _: TangentVector) {}

  @usableFromInline
  @derivative(of: method, wrt: x)
  func vjpMethod(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { $0 })
  }
}
