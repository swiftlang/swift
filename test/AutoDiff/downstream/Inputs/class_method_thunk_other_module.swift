class OtherModuleSuper {
  @differentiable
  func f(_ x: Float) -> Float {
    return 2 * x
  }

  @derivative(of: f)
  final func jvpf(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
    return (f(x), { v in 2 * v })
  }

  @derivative(of: f)
  final func vjpf(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    return (f(x), { v in 2 * v })
  }
}

class OtherModuleSubOverride : OtherModuleSuper {
  @differentiable
  override func f(_ x: Float) -> Float {
    return 3 * x
  }
}

class OtherModuleSubOverrideCustomDerivatives : OtherModuleSuper {
  @differentiable
  override func f(_ x: Float) -> Float {
    return 3 * x
  }

  @derivative(of: f)
  final func jvpf2(_ x: Float) -> (value: Float, differential: (Float) -> Float) {
    return (f(x), { v in 3 * v })
  }

  @derivative(of: f)
  final func vjpf2(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    return (f(x), { v in 3 * v })
  }
}
