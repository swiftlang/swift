import _Differentiation

public struct HasAutoDiffTypes {
  public var aFunction: @differentiable(reverse) (Float) -> Float
}
