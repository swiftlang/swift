import module1

extension FunctionInModule1_InternalDerivatives {
  // TODO(TF-1068): This causes duplicate symbol linker errors.
  // TODO(TF-1067): @usableFromInline should not be necessary.
  // @derivative(of: f)
  // @usableFromInline
  // static func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
  //   (x, { expectedGradientFromModule2 * $0 })
  // }
  // public static func gradFFromModule2(_ x: Float) -> Float { gradient(at: x, in: f) }
}

extension FunctionInModule1_PublicDerivativeInModule2 {
  @derivative(of: f)
  public static func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { expectedGradient * $0 })
  }
}

public extension FunctionInModule1_PublicDerivativeInOtherFile {
  static func gradFFromModule2(_ x: Float) -> Float { gradient(at: x, in: f) }
}
