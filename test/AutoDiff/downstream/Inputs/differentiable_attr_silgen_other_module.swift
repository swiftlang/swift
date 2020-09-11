public struct Wrapper : Differentiable, AdditiveArithmetic {
  public var x: Float
  public init(_ x: Float) {
    self.x = x
  }

  public static func + (lhs: Wrapper, rhs: Wrapper) -> Wrapper {
    return Wrapper(lhs.x + rhs.x)
  }

  @derivative(of: +)
  public static func vjpAdd(lhs: Wrapper, rhs: Wrapper)
    -> (value: Wrapper, pullback: (Wrapper) -> (Wrapper, Wrapper)) {
    return (lhs + rhs, { v in (v, v) })
  }

  public static func * (lhs: Wrapper, rhs: Wrapper) -> Wrapper {
    return Wrapper(lhs.x * rhs.x)
  }

  @derivative(of: *)
  public static func jvpMultiply(lhs: Wrapper, rhs: Wrapper)
    -> (value: Wrapper, differential: (Wrapper, Wrapper) -> Wrapper) {
    return (lhs * rhs, { dlhs, drhs in dlhs * rhs + lhs * drhs })
  }

  @derivative(of: *)
  public static func vjpMultiply(lhs: Wrapper, rhs: Wrapper)
    -> (value: Wrapper, pullback: (Wrapper) -> (Wrapper, Wrapper)) {
    return (lhs * rhs, { v in (v * rhs, v * lhs) })
  }
}
