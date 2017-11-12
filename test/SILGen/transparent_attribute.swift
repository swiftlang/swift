// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil -enable-sil-ownership %s | %FileCheck %s

// Test that the attribute gets set on default argument generators.

// CHECK-LABEL: sil hidden [transparent] @_T021transparent_attribute0A23FuncWithDefaultArgumentS2i1x_tFfA_ : $@convention(thin) () -> Int {

// CHECK-LABEL: sil hidden [transparent] @_T021transparent_attribute0A23FuncWithDefaultArgumentS2i1x_tF : $@convention(thin) (Int) -> Int {

@_transparent func transparentFuncWithDefaultArgument (x: Int = 1) -> Int {
  return x
}
func useTransparentFuncWithDefaultArgument() -> Int {
  return transparentFuncWithDefaultArgument()
}
