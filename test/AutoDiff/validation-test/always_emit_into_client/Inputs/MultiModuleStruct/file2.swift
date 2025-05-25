import MultiModuleStruct1
import _Differentiation

extension Struct : Differentiable {
  @_alwaysEmitIntoClient
  @derivative(of: sum)
  public func _vjpSum() -> (
    value: Float, pullback: (Float) -> Self.TangentVector
  ) {
    (value: self.x, pullback: { Self.TangentVector(42 * $0) })
  }

  @_alwaysEmitIntoClient
  @derivative(of: sum)
  public func _jvpSum() -> (
    value: Float, differential: (Self.TangentVector) -> Float
  ) {
    (value: self.x, differential: { 42 * $0.x })
  }
}
