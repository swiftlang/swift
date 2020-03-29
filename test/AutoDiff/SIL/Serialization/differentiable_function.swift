// RUN: %empty-directory(%t)
// RUN: %target-sil-opt -enable-experimental-differentiable-programming %s -emit-sib -o %t/tmp.sib -module-name main
// RUN: %target-sil-opt -enable-experimental-differentiable-programming %t/tmp.sib -o %t/tmp.sil -module-name main
// NOTE(SR-12090): Workaround because import declarations are not preserved in .sib files.
// RUN: sed -e 's/import Swift$/import Swift; import _Differentiation/' %t/tmp.sil > %t/tmp_fixed.sil
// RUN: %target-sil-opt -enable-experimental-differentiable-programming %t/tmp_fixed.sil -module-name main -emit-sorted-sil | %FileCheck %s

// NOTE(SR-12090): `shell` is required only to run `sed` as a SR-12090 workaround.
// REQUIRES: shell

sil_stage raw

import Swift
import _Differentiation

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

sil @c : $@convention(thin) (@differentiable (Float, @noDerivative Float) -> Float) -> @differentiable (Float, @noDerivative Float) -> Float {
bb0(%0 : $@differentiable (Float, @noDerivative Float) -> Float):
  return %0 : $@differentiable (Float, @noDerivative Float) -> Float
}

// CHECK-LABEL: sil @c : $@convention(thin) (@differentiable (Float, @noDerivative Float) -> Float) -> @differentiable (Float, @noDerivative Float) -> Float {
// CHECK: bb0(%0 : $@differentiable (Float, @noDerivative Float) -> Float):
// CHECK:   return %0 : $@differentiable (Float, @noDerivative Float) -> Float
// CHECK: }

sil @d : $@convention(thin) (@differentiable(linear) (Float, @noDerivative Float) -> Float) -> @differentiable(linear) (Float, @noDerivative Float) -> Float {
bb0(%0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float):
  return %0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float
}

// CHECK-LABEL: sil @d : $@convention(thin) (@differentiable(linear) (Float, @noDerivative Float) -> Float) -> @differentiable(linear) (Float, @noDerivative Float) -> Float {
// CHECK: bb0(%0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float):
// CHECK:   return %0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float
// CHECK: }
