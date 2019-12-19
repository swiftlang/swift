public enum FunctionInModule1_InternalDerivatives {
  public static let expectedGradientFromModule1: Float = 10
  public static let expectedGradientFromModule2: Float = 20
  public static let expectedGradientFromMain: Float = 30

  public static func f(_ x: Float) -> Float { x }

  // TODO(TF-1067): @usableFromInline should not be necessary.
  @derivative(of: f)
  @usableFromInline
  static func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { expectedGradientFromModule1 * $0 })
  }
  public static func gradFFromModule1(_ x: Float) -> Float { gradient(at: x, in: f) }
}

public enum FunctionInModule1_PublicDerivativeInOtherFile {
  public static let expectedGradient: Float = 10
  public static func f(_ x: Float) -> Float { x }
  public static func gradFFromModule1(_ x: Float) -> Float { gradient(at: x, in: f) }
}

public enum FunctionInModule1_PublicDerivativeInModule2 {
  public static let expectedGradient: Float = 10
  public static func f(_ x: Float) -> Float { x }
}

public enum FunctionInModule1_PublicDerivativeImplementedInternally {
  public static let expectedGradient: Float = 10

  @differentiable
  public static func f(_ x: Float) -> Float { x }

  @derivative(of: f)
  static func df(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    (x, { expectedGradient * $0 })
  }
  public static func gradFFromModule1(_ x: Float) -> Float { gradient(at: x, in: f) }
}
