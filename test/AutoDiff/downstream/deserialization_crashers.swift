// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sib %s -o %t/tmp.sib
// RUN: %target-sil-opt %t/tmp.sib

// TF-256: Crashes when deserializing witness thunk for requirement requiring
// differentiability wrt a subset of parameters.
protocol DifferentiableWRTSubset : Differentiable {
  @differentiable(wrt: (self))
  func f(x: Float) -> Float

  @differentiable(wrt: (x))
  func g(x: Float) -> Float
}

struct TF256 : DifferentiableWRTSubset {
  var param: Float = 0

  @differentiable(wrt: (self))
  func f(x: Float) -> Float { return x + param }

  @differentiable(wrt: (x))
  func g(x: Float) -> Float { return x + param }
}
