// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import DifferentiationUnittest
import StdlibUnittest

var InoutParameterAutoDiffTests = TestSuite("InoutParameterDifferentiation")

// SR-13305: Test function with non-wrt `inout` parameter, which should be
// treated as a differentiability result.

protocol SR_13305_Protocol {
  @differentiable(wrt: x)
  func method(_ x: Float, _ y: inout Float)

  @differentiable(wrt: x)
  func genericMethod<T: Differentiable>(_ x: T, _ y: inout T)
}

InoutParameterAutoDiffTests.test("non-wrt inout parameter") {
  struct SR_13305_Struct: SR_13305_Protocol {
    @differentiable(wrt: x)
    func method(_ x: Float, _ y: inout Float) {
      y = y * x
    }

    @differentiable(wrt: x)
    func genericMethod<T: Differentiable>(_ x: T, _ y: inout T) {
      y = x
    }
  }

  @differentiable(wrt: x)
  func foo(_ s: SR_13305_Struct, _ x: Float, _ y: Float) -> Float {
    var y = y
    s.method(x, &y)
    return y
  }

  @differentiable(wrt: x)
  func fooGeneric<T: SR_13305_Protocol>(_ s: T, _ x: Float, _ y: Float) -> Float {
    var y = y
    s.method(x, &y)
    return x
  }

  let s = SR_13305_Struct()

  do {
    let (value, (dx, dy)) = valueWithGradient(at: 2, 3, in: { foo(s, $0, $1) })
    expectEqual(6, value)
    expectEqual((3, 2), (dx, dy))
  }
  expectEqual((value: 6, gradient: 3), valueWithGradient(at: 2, in: { foo(s, $0, 3) }))

  do {
    let (value, (dx, dy)) = valueWithGradient(at: 2, 3, in: { fooGeneric(s, $0, $1) })
    expectEqual(2, value)
    expectEqual((1, 0), (dx, dy))
  }
  expectEqual((value: 2, gradient: 1), valueWithGradient(at: 2, in: { fooGeneric(s, $0, 3) }))
}

runAllTests()
