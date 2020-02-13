// RUN: %target-swift-frontend -emit-sil %S/Inputs/noderivative_attr_other_file.swift %s | %FileCheck %s

@differentiable
func bar(_ x: Float) -> Float {
  return Float(floatToIntNoDerivative(x))
}

// CHECK: sil hidden [_semantics "autodiff.nonvarying"] @float_to_int_noderivative : $@convention(thin) (Float) -> Int
