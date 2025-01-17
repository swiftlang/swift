// RUN: %empty-directory(%t)
// RUN: %target-sil-opt -sil-print-types %s -emit-sib -o %t/tmp.sib -module-name main
// RUN: %target-sil-opt -sil-print-types %t/tmp.sib -o %t/tmp.sil -module-name main

// https://github.com/apple/swift/issues/54526
// Workaround because import declarations are not preserved in .sib files.
// RUN: sed -e 's/import Swift$/import Swift; import _Differentiation/' %t/tmp.sil > %t/tmp_fixed.sil
// RUN: %target-sil-opt -sil-print-types %t/tmp_fixed.sil -module-name main -emit-sorted-sil | %FileCheck %s

// `shell` is required only to run `sed` as a
// https://github.com/apple/swift/issues/54526 workaround.
// REQUIRES: shell

sil_stage raw

import Swift
import _Differentiation

// Normal `@differentiable` function type.
sil @a : $@convention(thin) (@differentiable(reverse) (Float) -> Float) -> @differentiable(reverse) (Float) -> Float {
bb0(%0 : $@differentiable(reverse) (Float) -> Float):
  return %0 : $@differentiable(reverse) (Float) -> Float
}

// CHECK-LABEL: sil @a : $@convention(thin) (@differentiable(reverse) (Float) -> Float) -> @differentiable(reverse) (Float) -> Float {
// CHECK: bb0([[ARG:%.*]] : $@differentiable(reverse) (Float) -> Float):
// CHECK:   return [[ARG]] : $@differentiable(reverse) (Float) -> Float
// CHECK: }

// Normal `@differentiable` function type with `@noDerivative` parameters.
sil @c : $@convention(thin) (@differentiable(reverse) (Float, @noDerivative Float) -> Float) -> @differentiable(reverse) (Float, @noDerivative Float) -> Float {
bb0(%0 : $@differentiable(reverse) (Float, @noDerivative Float) -> Float):
  return %0 : $@differentiable(reverse) (Float, @noDerivative Float) -> Float
}

// CHECK-LABEL: sil @c : $@convention(thin) (@differentiable(reverse) (Float, @noDerivative Float) -> Float) -> @differentiable(reverse) (Float, @noDerivative Float) -> Float {
// CHECK: bb0(%0 : $@differentiable(reverse) (Float, @noDerivative Float) -> Float):
// CHECK:   return %0 : $@differentiable(reverse) (Float, @noDerivative Float) -> Float
// CHECK: }

// Normal `@differentiable` function type with `@noDerivative` parameters and results.
sil @e : $@convention(thin) (@differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float)) -> @differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float) {
bb0(%0 : $@differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float)):
  return %0 : $@differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float)
}

// CHECK-LABEL: sil @e : $@convention(thin) (@differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float)) -> @differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float) {
// CHECK: bb0(%0 : $@differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float)):
// CHECK:   return %0 : $@differentiable(reverse) (Float, @noDerivative Float) -> (Float, @noDerivative Float)
// CHECK: }
