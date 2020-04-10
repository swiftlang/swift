import _Differentiation

@derivative(of: id)
func derivative(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  fatalError()
}
