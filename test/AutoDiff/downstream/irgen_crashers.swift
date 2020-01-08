// RUN: %target-swift-frontend -emit-ir %s

// TF-917: `partial_apply` IRGen crash.
public protocol TF_917: Differentiable {
  @differentiable
  func r<A>(_ a: A) -> Float
}
@differentiable
public func tf_917<B: TF_917>(_ b: B) -> Float {
  return b.r(0.0)
}

