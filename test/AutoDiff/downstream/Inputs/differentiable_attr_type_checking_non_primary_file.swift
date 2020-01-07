public protocol Layer: Differentiable {
  associatedtype Input: Differentiable
  associatedtype Output: Differentiable

  @differentiable
  func instanceMethod(_ input: Input) -> Output

  @differentiable
  var computedProperty: Output { get }
}

struct DummyLayer: Layer {
  @differentiable
  func instanceMethod(_ input: Float) -> Float {
    return input
  }

  @differentiable
  var computedProperty: Float { 1 }
}

public extension Differentiable {
  @differentiable
  func sequenced<L: Layer>(through layer: L) -> L.Output where L.Input == Self {
    return layer.instanceMethod(self)
  }
}
