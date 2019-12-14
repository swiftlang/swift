extension FunctionInModule1_PublicDerivativeInOtherFile {
  @derivative(of: f)
  public static func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { expectedGradient * $0 })
  }
  public static func gradFFromModule1OtherFile(_ x: Float) -> Float { gradient(at: x, in: f) }
}
