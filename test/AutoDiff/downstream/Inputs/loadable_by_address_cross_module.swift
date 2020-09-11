public struct LargeLoadableType<T>: AdditiveArithmetic, Differentiable {
  public var a, b, c, d, e: Float

  public init(a: Float) {
    self.a = a
    self.b = 0
    self.c = 0
    self.d = 0
    self.e = 0
  }

  @differentiable
  public func externalLBAModifiedFunction(_ x: Float) -> Float {
    return a * x
  }
}
