// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var DiffProtoReqTests = TestSuite("DiffProtoReq")

protocol Proto {
  @differentiable(reverse)
  func f(_ x: Float) -> Float
}

func callF<T: Proto>(_ t: T, _ x: Float) -> Float {
  return t.f(x)
}

func gradFWrtX<T: Proto>(_ t: T, at x: Float) -> Float {
  return (#gradient(callF, wrt: .1) as (T, Float) -> Float)(t, x)
}

struct MultiplyConstant : Proto {
  let constant: Float

  @differentiable(reverse, adjoint: fAdj)
  func f(_ x: Float) -> Float {
    return constant * x
  }

  func fAdj(_ x: Float, _ origResult: Float, _ seed: Float) -> Float {
    return constant * seed
  }
}

DiffProtoReqTests.test("gradient") {
  expectEqual(10.0, gradFWrtX(MultiplyConstant(constant: 10), at: 0))
}

runAllTests()
