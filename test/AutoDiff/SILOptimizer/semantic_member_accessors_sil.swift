// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -Xllvm -sil-print-after=differentiation %s -module-name null -o /dev/null 2>&1 | %FileCheck %s

// Test differentiation of semantic member accessors:
// - Stored property accessors.
// - Property wrapper wrapped value accessors.

// TODO(TF-1254): Support forward-mode differentiation and test generated differentials.

import _Differentiation

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

struct Struct: Differentiable {
  @Wrapper @Wrapper var x: Float = 10
  var y: Float = 10
}

struct Generic<T> {
  @Wrapper @Wrapper var x: T
  var y: T
}
extension Generic: Differentiable where T: Differentiable {}

func trigger<T: Differentiable>(_ x: T.Type) {
  let _: @differentiable(reverse) (Struct) -> Float = { $0.x }
  let _: @differentiable(reverse) (inout Struct, Float) -> Void = { $0.x = $1 }

  let _: @differentiable(reverse) (Generic<T>) -> T = { $0.x }
  let _: @differentiable(reverse) (inout Generic<T>, T) -> Void = { $0.x = $1 }
}

// CHECK-LABEL: // differentiability witness for Generic.x.setter
// CHECK-NEXT: sil_differentiability_witness private [reverse] [parameters 0 1] [results 0] <τ_0_0 where τ_0_0 : Differentiable> @$s4null7GenericV1xxvs : $@convention(method) <T> (@in T, @inout Generic<T>) -> () {

// CHECK-LABEL: // differentiability witness for Generic.x.getter
// CHECK-NEXT: sil_differentiability_witness private [reverse] [parameters 0] [results 0] <τ_0_0 where τ_0_0 : Differentiable> @$s4null7GenericV1xxvg : $@convention(method) <T> (@in_guaranteed Generic<T>) -> @out T {

// CHECK-LABEL: // differentiability witness for Struct.x.setter
// CHECK-NEXT: sil_differentiability_witness private [reverse] [parameters 0 1] [results 0] @$s4null6StructV1xSfvs : $@convention(method) (Float, @inout Struct) -> () {

// CHECK-LABEL: // differentiability witness for Struct.x.getter
// CHECK-NEXT: sil_differentiability_witness private [reverse] [parameters 0] [results 0] @$s4null6StructV1xSfvg : $@convention(method) (Struct) -> Float {

// CHECK-LABEL: sil private [ossa] @$s4null7GenericV1xxvs16_Differentiation14DifferentiableRzlTJpSSpSr
// CHECK: bb0([[ADJ_X_RESULT:%.*]] : $*τ_0_0.TangentVector, [[ADJ_SELF:%.*]] : $*Generic<τ_0_0>.TangentVector):
// CHECK:   [[ADJ_X_TMP:%.*]] = alloc_stack $τ_0_0.TangentVector
// CHECK:   [[ZERO_FN:%.*]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK:   apply [[ZERO_FN]]<τ_0_0.TangentVector>([[ADJ_X_TMP]], {{.*}})
// CHECK:   [[ADJ_X:%.*]] = struct_element_addr [[ADJ_SELF]] : $*Generic<τ_0_0>.TangentVector, #Generic.TangentVector.x
// CHECK:   [[ZERO_FN:%.*]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic."+="
// CHECK:   apply [[ZERO_FN]]<τ_0_0.TangentVector>([[ADJ_X_TMP]], [[ADJ_X]], {{.*}})
// CHECK:   destroy_addr [[ADJ_X]] : $*τ_0_0.TangentVector
// CHECK:   [[ZERO_FN:%.*]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK:   apply [[ZERO_FN]]<τ_0_0.TangentVector>([[ADJ_X]], {{.*}})
// CHECK:   copy_addr [take] [[ADJ_X_TMP]] to [init] [[ADJ_X_RESULT]] : $*τ_0_0.TangentVector
// CHECK:   dealloc_stack [[ADJ_X_TMP]] : $*τ_0_0.TangentVector
// CHECK:   return {{.*}} : $()
// CHECK: }

// CHECK-LABEL: sil private [ossa] @$s4null7GenericV1xxvg16_Differentiation14DifferentiableRzlTJpSpSr
// CHECK: bb0([[ADJ_SELF_RESULT:%.*]] : $*Generic<τ_0_0>.TangentVector, [[SEED:%.*]] : $*τ_0_0.TangentVector):
// CHECK:   [[ADJ_SELF_TMP:%.*]] = alloc_stack $Generic<τ_0_0>.TangentVector
// CHECK:   [[SEED_COPY:%.*]] = alloc_stack $τ_0_0.TangentVector
// CHECK:   copy_addr [[SEED]] to [init] [[SEED_COPY]] : $*τ_0_0.TangentVector
// CHECK:   [[ADJ_X:%.*]] = struct_element_addr [[ADJ_SELF_TMP]] : $*Generic<τ_0_0>.TangentVector, #Generic.TangentVector.x
// CHECK:   copy_addr [take] [[SEED_COPY]] to [init] [[ADJ_X]] : $*τ_0_0.TangentVector
// CHECK:   [[ADJ_Y:%.*]] = struct_element_addr [[ADJ_SELF_TMP]] : $*Generic<τ_0_0>.TangentVector, #Generic.TangentVector.y
// CHECK:   [[ZERO_FN:%.*]] = witness_method $τ_0_0.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK:   apply [[ZERO_FN]]<τ_0_0.TangentVector>([[ADJ_Y]], {{.*}})
// CHECK:   copy_addr [take] [[ADJ_SELF_TMP]] to [init] [[ADJ_SELF_RESULT]] : $*Generic<τ_0_0>.TangentVector
// CHECK:   dealloc_stack [[SEED_COPY]] : $*τ_0_0.TangentVector
// CHECK:   dealloc_stack [[ADJ_SELF_TMP]] : $*Generic<τ_0_0>.TangentVector
// CHECK:   return {{.*}} : $()
// CHECK: }

// CHECK-LABEL: sil private [ossa] @$s4null6StructV1xSfvsTJpSSpSr
// CHECK: bb0([[ADJ_SELF:%.*]] : $*Struct.TangentVector):
// CHECK:   [[ADJ_X_ADDR:%.*]] = struct_element_addr [[ADJ_SELF]] : $*Struct.TangentVector, #Struct.TangentVector.x
// CHECK:   [[ADJ_X:%.*]] = load [trivial] [[ADJ_X_ADDR]] : $*Float
// CHECK:   [[ZERO_FN:%.*]] = witness_method $Float, #AdditiveArithmetic.zero!getter
// CHECK:   apply [[ZERO_FN]]<Float>([[ADJ_X_ADDR]], {{.*}})
// CHECK:   return [[ADJ_X]] : $Float
// CHECK: }

// CHECK-LABEL: sil private [ossa] @$s4null6StructV1xSfvgTJpSpSr
// CHECK: bb0([[ADJ_X:%.*]] : $Float):
// CHECK:   [[ADJ_Y_ADDR:%.*]] = alloc_stack $Float
// CHECK:   [[ZERO_FN:%.*]] = witness_method $Float, #AdditiveArithmetic.zero!getter
// CHECK:   apply [[ZERO_FN]]<Float>([[ADJ_Y_ADDR]], {{.*}})
// CHECK:   [[ADJ_Y:%.*]] = load [trivial] [[ADJ_Y_ADDR]] : $*Float
// CHECK:   dealloc_stack [[ADJ_Y_ADDR]] : $*Float
// CHECK:   [[ADJ_SELF:%.*]] = struct $Struct.TangentVector ([[ADJ_X]] : $Float, [[ADJ_Y]] : $Float)
// CHECK:   return [[ADJ_SELF]] : $Struct.TangentVector
// CHECK: }
