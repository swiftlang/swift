struct TF_619: Differentiable {
  var p: Float = 1

  @differentiable
  func foo(_ x: Float) -> Float {
    return p * x
  }

  @derivative(of: foo)
  func vjpFoo(_ x: Float) -> (value: Float, pullback: (Float) -> (TangentVector, Float)) {
    return (x, { v in (TangentVector(p: v * x), v * self.p) })
  }
}
