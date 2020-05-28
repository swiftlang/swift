import _Differentiation
import a

extension Struct: Differentiable {
  public struct TangentVector: Differentiable & AdditiveArithmetic {}
  public mutating func move(along _: TangentVector) {}
  public var zeroTangentVectorInitializer: () -> TangentVector { { .zero } }

  @usableFromInline
  @derivative(of: method, wrt: x)
  func vjpMethod(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { $0 })
  }
}
