// RUN: %empty-directory(%t)
// RUN: %target-sil-opt %s -emit-sib -o %t/tmp.sib -module-name main
// RUN: %target-sil-opt %t/tmp.sib -o %t/tmp.sil -module-name main
// NOTE(SR-12090): Workaround because import declarations are not preserved in .sib files.
// RUN: sed -e 's/import Swift$/import Swift; import _Differentiation/' %t/tmp.sil > %t/tmp_fixed.sil
// RUN: %target-sil-opt %t/tmp_fixed.sil -module-name main -emit-sorted-sil | %FileCheck %s

// NOTE(SR-12090): `shell` is required only to run `sed` as a SR-12090 workaround.
// REQUIRES: shell

sil_stage raw

import Swift
import _Differentiation

// Normal `@differentiable` function type.
sil @a : $@convention(thin) (@differentiable (Float) -> Float) -> @differentiable (Float) -> Float {
bb0(%0 : $@differentiable (Float) -> Float):
  return %0 : $@differentiable (Float) -> Float
}

// CHECK-LABEL: sil @a : $@convention(thin) (@differentiable (Float) -> Float) -> @differentiable (Float) -> Float {
// CHECK: bb0([[ARG:%.*]] : $@differentiable (Float) -> Float):
// CHECK:   return [[ARG]] : $@differentiable (Float) -> Float
// CHECK: }

// Linear `@differentiable` function type.
sil @b : $@convention(thin) (@differentiable(linear) (Float) -> Float) -> @differentiable(linear) (Float) -> Float {
bb0(%0 : $@differentiable(linear) (Float) -> Float):
  return %0 : $@differentiable(linear) (Float) -> Float
}

// CHECK-LABEL: sil @b : $@convention(thin) (@differentiable(linear) (Float) -> Float) -> @differentiable(linear) (Float) -> Float {
// CHECK: bb0([[ARG:%.*]] : $@differentiable(linear) (Float) -> Float):
// CHECK:   return [[ARG]] : $@differentiable(linear) (Float) -> Float
// CHECK: }

// Normal `@differentiable` function type with `@noDerivative` parameters.
sil @c : $@convention(thin) (@differentiable (Float, @noDerivative Float) -> Float) -> @differentiable (Float, @noDerivative Float) -> Float {
bb0(%0 : $@differentiable (Float, @noDerivative Float) -> Float):
  return %0 : $@differentiable (Float, @noDerivative Float) -> Float
}

// CHECK-LABEL: sil @c : $@convention(thin) (@differentiable (Float, @noDerivative Float) -> Float) -> @differentiable (Float, @noDerivative Float) -> Float {
// CHECK: bb0(%0 : $@differentiable (Float, @noDerivative Float) -> Float):
// CHECK:   return %0 : $@differentiable (Float, @noDerivative Float) -> Float
// CHECK: }

// Linear `@differentiable` function type with `@noDerivative` parameters.
sil @d : $@convention(thin) (@differentiable(linear) (Float, @noDerivative Float) -> Float) -> @differentiable(linear) (Float, @noDerivative Float) -> Float {
bb0(%0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float):
  return %0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float
}

// CHECK-LABEL: sil @d : $@convention(thin) (@differentiable(linear) (Float, @noDerivative Float) -> Float) -> @differentiable(linear) (Float, @noDerivative Float) -> Float {
// CHECK: bb0(%0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float):
// CHECK:   return %0 : $@differentiable(linear) (Float, @noDerivative Float) -> Float
// CHECK: }

// Normal `@differentiable` function type with `@noDerivative` parameters and results.
sil @e : $@convention(thin) (@differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float)) -> @differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float) {
bb0(%0 : $@differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float)):
  return %0 : $@differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float)
}

// CHECK-LABEL: sil @e : $@convention(thin) (@differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float)) -> @differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float) {
// CHECK: bb0(%0 : $@differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float)):
// CHECK:   return %0 : $@differentiable (Float, @noDerivative Float) -> (Float, @noDerivative Float)
// CHECK: }

// Linear `@differentiable` function type with `@noDerivative` parameters and results.
sil @f : $@convention(thin) (@differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float)) -> @differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float) {
bb0(%0 : $@differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float)):
  return %0 : $@differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float)
}

// CHECK-LABEL: sil @f : $@convention(thin) (@differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float)) -> @differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float) {
// CHECK: bb0(%0 : $@differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float)):
// CHECK:   return %0 : $@differentiable(linear) (Float, @noDerivative Float) -> (Float, @noDerivative Float)
// CHECK: }
