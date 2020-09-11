// RUN: %target-swift-emit-sil %s
// REQUIRES: asserts

// TF-922: Adjoint value type mismatch assertion failure during direct adjoint
// accumulation in differentiation transform pullback generation.

// The cause is an adjoint value accumulation bug in
// `PullbackEmitter::visitDestructureTupleInst` for tuple values with
// non-tuple-typed adjoint values. This is relevant for array literal
// initialization because `array.uninitialized_intrinsic` returns a tuple
// with a `destructure_tuple` user.

@differentiable
func TF_922(_ x: Float) -> [Float] {
  var result = [x]
  let result2 = true ? result : result
  let result3 = true ? result : result2
  return result3
}

// Assertion failed: (lhs->getType() == rhs->getType() && "Adjoints must have equal types!"), function accumulateDirect, file swift/lib/SILOptimizer/Mandatory/Differentiation.cpp, line 7654.
// Stack dump:
// ...
// 1.	Swift version 5.1.1-dev (Swift d89e9d1881)
// 2.	While running pass #59 SILModuleTransform "Differentiation".
// 3.	While canonicalizing `differentiable_function` SIL node   %12 = differentiable_function [parameters 0] %11 : $@callee_guaranteed (Float) -> @owned Array<Float> // users: %30, %13
// 4.	While ...in SIL function "@main".
// 5.	While processing `[differentiable source 0 wrt 0]` attribute on SIL function "@$s4main17oneElementLiteralySaySfGSfF".
//  for 'oneElementLiteral(_:)' (at tf-922-array.swift:2:1)
