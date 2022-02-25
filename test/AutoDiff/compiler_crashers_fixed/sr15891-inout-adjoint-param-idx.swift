// RUN: %target-swift-frontend -emit-sil -verify %s

// SR-15891: The parameter indices used for copying
// inout tangent vectors were calculated improperly
// in presence of other pullback parameters (e.g. 
// captures)

import _Differentiation

struct Foo {
  var bar : Float
  var baz : Float
  var name : String?
}

func outerFunc(doIterations : Int, value: inout Float) -> (Float, (Float) -> Float) {
  @differentiable(reverse, wrt: param)
  func innerFunc1(param: Float, other: Foo) -> Float {
    value += param * other.bar
    return value * param * 2.0
  }
  
  @differentiable(reverse, wrt: param1)
  func loop(param1 : Float, other1: Foo) -> Float {
    var res : Float;
    res = 0.0
    if (doIterations > 0) {
      res = innerFunc1(param: param1, other: other1)
    }
    
    return res
  }
  
  @differentiable(reverse)
  func curriedFunc(param: Float) -> Float {
    let other = Foo(bar: 7, baz: 9)
    return loop(param1: param, other1: other)
  }
  
  let valAndPullback = valueWithPullback(at: value, of: curriedFunc)
  return (value + valAndPullback.value, valAndPullback.pullback)
}
