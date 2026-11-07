import _Differentiation

@frozen
public struct Pair<T: Differentiable>: Differentiable {
  public var a: T
  public var b: T
  public init(a: T, b: T) {
    self.a = a
    self.b = b
  }
}

public final class Box<T: Differentiable>: Differentiable {
  public var x: T
  public init(x: T) { self.x = x }
}
