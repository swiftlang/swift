// RUN: %target-swift-frontend -emit-silgen %S/Inputs/noderivative_attr_other_file.swift %s | %FileCheck %s

import _Differentiation

@differentiable
func bar(_ x: Float) -> Float {
  return Float(floatToIntNoDerivative(x))
}

// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @float_to_int_noderivative : $@convention(thin) (Float) -> Int
