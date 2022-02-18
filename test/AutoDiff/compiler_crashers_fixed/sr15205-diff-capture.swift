// RUN: %target-swift-frontend -emit-sil -verify %s

// SR-15205: fix assertions related to captured arguments, they should
// treated as constants

import _Differentiation

func outerFunc(value: inout Float) -> (Float, (Float) -> (Float, Float)) {
  
  @differentiable(reverse, wrt: param)
  func innerFunc(param: Float, other: Float) -> Float {
    value += param * other
    return value * param * 2.0
  }
  
  let valAndPullback = valueWithPullback(at: value, 2.0, of: innerFunc)
  return (value + valAndPullback.value, valAndPullback.pullback)
}

func outerFunc2(value: inout Float) -> (Float, (Float) -> Float) {

  @differentiable(reverse, wrt: param)
  func innerFunc(param: Float, other: Float) -> Float {
    value += param * other
    return value * param * 2.0
  }

  @differentiable(reverse)
  func curriedFunc(param: Float) -> Float {
    return innerFunc(param: param, other: 3.0)
  }

  let valAndPullback = valueWithPullback(at: value, of: curriedFunc)
  return (value + valAndPullback.value, valAndPullback.pullback)
}

