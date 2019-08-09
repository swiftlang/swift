class OtherModuleSuper {
  @differentiable(jvp: jvpf, vjp: vjpf)
  func f(_ x: Float) -> Float {
    return 2 * x
  }

  final func jvpf(_ x: Float) -> (Float, (Float) -> Float) {
    return (f(x), { v in 2 * v })
  }

  final func vjpf(_ x: Float) -> (Float, (Float) -> Float) {
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
  @differentiable(jvp: jvpf2, vjp: vjpf2)
  override func f(_ x: Float) -> Float {
    return 3 * x
  }

  final func jvpf2(_ x: Float) -> (Float, (Float) -> Float) {
    return (f(x), { v in 3 * v })
  }

  final func vjpf2(_ x: Float) -> (Float, (Float) -> Float) {
    return (f(x), { v in 3 * v })
  }
}
