// RUN: %empty-directory(%t)
// RUN: %target-sil-opt %s -emit-sib -o %t/tmp.sib -module-name differentiation -enable-experimental-differentiable-programming
// RUN: %target-sil-opt %t/tmp.sib -o %t/tmp.2.sib -module-name differentiation -enable-experimental-differentiable-programming
// RUN: %target-sil-opt %t/tmp.2.sib -module-name differentiation -emit-sorted-sil -enable-experimental-differentiable-programming | %FileCheck %s

sil_stage raw

import Swift

sil @a : $@convention(thin) (@differentiable (Float) -> Float) -> @differentiable (Float) -> Float {
bb0(%0 : $@differentiable (Float) -> Float):
  return %0 : $@differentiable (Float) -> Float
}

// CHECK-LABEL: sil @a : $@convention(thin) (@differentiable (Float) -> Float) -> @differentiable (Float) -> Float {
// CHECK: bb0([[ARG:%.*]] : $@differentiable (Float) -> Float):
// CHECK:   return [[ARG]] : $@differentiable (Float) -> Float
// CHECK: }

sil @b : $@convention(thin) (@differentiable(linear) (Float) -> Float) -> @differentiable(linear) (Float) -> Float {
bb0(%0 : $@differentiable(linear) (Float) -> Float):
  return %0 : $@differentiable(linear) (Float) -> Float
}

// CHECK-LABEL: sil @b : $@convention(thin) (@differentiable(linear) (Float) -> Float) -> @differentiable(linear) (Float) -> Float {
// CHECK: bb0([[ARG:%.*]] : $@differentiable(linear) (Float) -> Float):
// CHECK:   return [[ARG]] : $@differentiable(linear) (Float) -> Float
// CHECK: }
