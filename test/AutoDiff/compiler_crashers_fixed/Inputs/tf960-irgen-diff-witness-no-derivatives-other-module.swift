struct TF_960: Differentiable {
  @differentiable
  func callAsFunction(_ input: Float) -> Float {
    return input
  }
}
