// RUN: %target-swift-emit-sil %s
// REQUIRES: asserts

// TF-945: Activity analysis crash because
// `DifferentiableActivityInfo::getLookupConformanceFunction` returned a
// `LookupConformanceFn` (type alias for `llvm::function_ref`), which does not
// own the underlying callable.

@differentiable
func TF_945(_ x: Float) -> Float {
  var result = (x, 1)
  let (x, y) = result
  return x
}
