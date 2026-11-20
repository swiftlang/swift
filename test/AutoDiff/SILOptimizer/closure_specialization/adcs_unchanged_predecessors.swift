// RUN: %target-swift-frontend -O -sil-verify-all -emit-sil %s | %FileCheck %s --implicit-check-not='enum _AD__{{[^ ]+}}_spec_'
// RUN: %target-swift-frontend -O -emit-ir -disable-llvm-optzns %s | %FileCheck %s --check-prefix=IR

import _Differentiation

// Only the merge of the switch arms stores callbacks. Its specialized cases
// must keep the original predecessor types throughout the switch.
// CHECK: enum _AD__{{[^ ]+sparse[^ ]+}}_spec_bb{{[^ ]+}} {
// CHECK: case bb[[#]]((predecessor: {{_AD__[^ ,)]+__Pred__src_0_wrt_0}}, (), (Float, Float)))
@differentiable(reverse, wrt: x)
public func sparse(_ x: Float, _ tag: Int) -> Float {
  switch tag {
  case 0: return (x + 1) * x
  case 1: return (x + 2) * x
  case 2: return (x + 3) * x
  default: return x
  }
}

// Both successors require a new type even though they only carry a predecessor.
// CHECK:      enum [[INNER:_AD__[^ ]+nested[^ ]+_spec_bb[^ ]+]] {
// CHECK:      enum [[LEFT:_AD__[^ ]+nested[^ ]+_spec_[^ ]+]] {
// CHECK-NEXT: case bb[[#]]((predecessor: [[INNER]]))
// CHECK:      enum [[RIGHT:_AD__[^ ]+nested[^ ]+_spec_[^ ]+]] {
// CHECK-NEXT: case bb[[#]]((predecessor: [[INNER]]))
// CHECK:      enum {{_AD__[^ ]+nested[^ ]+_spec_bb[^ ]+}} {
// CHECK-DAG:  predecessor: [[LEFT]]
// CHECK-DAG:  predecessor: [[RIGHT]]
@differentiable(reverse, wrt: x)
public func nested(_ x: Float, _ first: Bool, _ second: Bool) -> Float {
  let interim = first ? x * x : x + x
  return second ? interim * x : interim
}

// CHECK:   // function_ref specialized pullback of sparse(_:_:)
// CHECK:   // function_ref specialized pullback of nested(_:_:_:)

// IR:     ; ModuleID =
// IR-NOT: @"{{[^"]*_spec_[^"]*}}O{{Mn|N}}" = {{(protected |dllexport |dso_local )*(constant|alias)}}
// IR-NOT: define {{(protected |dllexport |dso_local )*}}swiftcc {{[^@]*}}@"{{[^"]*_spec_[^"]*}}OMa"
