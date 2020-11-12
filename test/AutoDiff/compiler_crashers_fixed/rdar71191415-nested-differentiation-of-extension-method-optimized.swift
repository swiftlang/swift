// RUN: %target-build-swift -O %s

// rdar://71191415

import _Differentiation

protocol P {
  @differentiable
  func req(_ input: Float) -> Float
}

extension P {
  @differentiable
  func foo(_ input: Float) -> Float {
    return req(input)
  }
}

struct Dummy: P {
  @differentiable
  func req(_ input: Float) -> Float {
    input
  }
}

struct DummyComposition: P {
  var layer = Dummy()

  @differentiable
  func req(_ input: Float) -> Float {
    layer.foo(input)
  }
}
