// RUN: %target-build-swift -O %s

// FIXME(rdar://89055298)
// UNSUPPORTED: OS=linux-gnu

// rdar://71191415

import _Differentiation

protocol P {
  @differentiable(reverse)
  func req(_ input: Float) -> Float
}

extension P {
  @differentiable(reverse)
  func foo(_ input: Float) -> Float {
    return req(input)
  }
}

struct Dummy: P {
  @differentiable(reverse)
  func req(_ input: Float) -> Float {
    input
  }
}

struct DummyComposition: P {
  var layer = Dummy()

  @differentiable(reverse)
  func req(_ input: Float) -> Float {
    layer.foo(input)
  }
}
