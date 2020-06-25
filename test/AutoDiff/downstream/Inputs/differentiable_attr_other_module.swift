// Verify that `@differentiable` declarations can be differentiated from other
// modules.

public struct Foo: Differentiable {
  public var x: Float

  @differentiable
  public init(_ x: Float) {
    self.x = x
  }

  @differentiable
  public func method() -> Float {
    x
  }

  @differentiable
  public var computedProperty: Float {
    x
  }

  @differentiable
  public subscript() -> Float {
    x
  }
}
