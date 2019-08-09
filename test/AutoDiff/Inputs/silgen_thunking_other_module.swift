struct TF_619: Differentiable {
  var p: Float = 1

  @differentiable(vjp: vjpFoo)
  func foo(_ x: Float) -> Float {
    return p * x
  }

  func vjpFoo(_ x: Float) -> (Float, (Float) -> (TangentVector, Float)) {
    return (x, { v in (TangentVector(p: v * x), v * self.p) })
  }
}
