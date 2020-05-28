// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// Check `Differentiable.zeroTangentVectorInitializer` derivation.
// There are two case:
// 1. Memberwise derivation.
// 2. `{ TangentVector.zero }` fallback derivation.

import _Differentiation

struct MemberwiseTangentVector: Differentiable {
  var x: Float
  var y: Double

  // Expected memberwise synthesis (1):
  // var zeroTangentVectorInitializer: () -> TangentVector {
  //   { [xZeroTanInit = x.zeroTangentVectorInitializer,
  //      yZeroTanInit = y.zeroTangentVectorInitializer] in
  //     return TangentVector(x: xZeroTanInit(), y: yZeroTanInit())
  //   }
  // }
}

struct SelfTangentVector: Differentiable & AdditiveArithmetic {
  var x: Float
  var y: Double
  typealias TangentVector = Self

  // Expected memberwise synthesis (1):
  // var zeroTangentVectorInitializer: () -> TangentVector {
  //   { [xZeroTanInit = x.zeroTangentVectorInitializer,
  //      yZeroTanInit = y.zeroTangentVectorInitializer] in
  //     return TangentVector(x: xZeroTanInit(), y: yZeroTanInit())
  //   }
  // }
}

struct CustomTangentVector<T: Differentiable, U: Differentiable>: Differentiable {
  var x: T
  var y: U

  typealias TangentVector = T.TangentVector
  mutating func move(along direction: TangentVector) {}

  // Expected fallback synthesis (2):
  // var zeroTangentVectorInitializer: () -> TangentVector {
  //   { TangentVector.zero }
  // }
}

// CHECK-LABEL: // MemberwiseTangentVector.zeroTangentVectorInitializer.getter
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}23MemberwiseTangentVectorV0bgH11InitializerAC0gH0Vycvg : $@convention(method) (MemberwiseTangentVector) -> @owned @callee_guaranteed () -> MemberwiseTangentVector.TangentVector {
// CHECK: bb0([[SELF:%.*]] : $MemberwiseTangentVector):
// CHECK:   [[X_PROP:%.*]] = struct_extract [[SELF]] : $MemberwiseTangentVector, #MemberwiseTangentVector.x
// CHECK:   [[X_ZERO_INIT_FN:%.*]] = function_ref @$sSf{{.*}}E28zeroTangentVectorInitializerSfycvg : $@convention(method) (Float) -> @owned @callee_guaranteed () -> Float
// CHECK:   [[X_ZERO_INIT:%.*]] = apply [[X_ZERO_INIT_FN]]([[X_PROP]])
// CHECK:   [[Y_PROP:%.*]] = struct_extract [[SELF]] : $MemberwiseTangentVector, #MemberwiseTangentVector.y
// CHECK:   [[Y_ZERO_INIT_FN:%.*]] = function_ref @$sSd{{.*}}E28zeroTangentVectorInitializerSdycvg : $@convention(method) (Double) -> @owned @callee_guaranteed () -> Double
// CHECK:   [[Y_ZERO_INIT:%.*]] = apply [[Y_ZERO_INIT_FN]]([[Y_PROP]])
// CHECK:   // function_ref closure #1 in MemberwiseTangentVector.zeroTangentVectorInitializer.getter
// CHECK:   [[CLOSURE_FN:%.*]] = function_ref @${{.*}}23MemberwiseTangentVectorV0bgH11InitializerAC0gH0VycvgAFycfU_
// CHECK:   [[X_ZERO_INIT_COPY:%.*]] = copy_value [[X_ZERO_INIT]]
// CHECK:   [[Y_ZERO_INIT_COPY:%.*]] = copy_value [[Y_ZERO_INIT]]
// CHECK:   [[ZERO_INIT:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]([[X_ZERO_INIT_COPY]], [[Y_ZERO_INIT_COPY]])
// CHECK:   return [[ZERO_INIT]] : $@callee_guaranteed () -> MemberwiseTangentVector.TangentVector
// CHECK: }

// CHECK-LABEL: // SelfTangentVector.zeroTangentVectorInitializer.getter
// CHECK: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}17SelfTangentVectorV0bgH11InitializerACycvg : $@convention(method) (SelfTangentVector) -> @owned @callee_guaranteed () -> SelfTangentVector {
// CHECK: bb0([[SELF:%.*]] : $SelfTangentVector):
// CHECK:   [[X_PROP:%.*]] = struct_extract [[SELF]] : $SelfTangentVector, #SelfTangentVector.x
// CHECK:   [[X_ZERO_INIT_FN:%.*]] = function_ref @$sSf{{.*}}E28zeroTangentVectorInitializerSfycvg : $@convention(method) (Float) -> @owned @callee_guaranteed () -> Float
// CHECK:   [[X_ZERO_INIT:%.*]] = apply [[X_ZERO_INIT_FN]]([[X_PROP]])
// CHECK:   [[Y_PROP:%.*]] = struct_extract [[SELF]] : $SelfTangentVector, #SelfTangentVector.y
// CHECK:   [[Y_ZERO_INIT_FN:%.*]] = function_ref @$sSd{{.*}}E28zeroTangentVectorInitializerSdycvg : $@convention(method) (Double) -> @owned @callee_guaranteed () -> Double
// CHECK:   [[Y_ZERO_INIT:%.*]] = apply [[Y_ZERO_INIT_FN]]([[Y_PROP]])
// CHECK:   // function_ref closure #2 in SelfTangentVector.zeroTangentVectorInitializer.getter
// CHECK:   [[CLOSURE_FN:%.*]] = function_ref @${{.*}}17SelfTangentVectorV0bgH11InitializerACycvgACycfU0_
// CHECK:   [[X_ZERO_INIT_COPY:%.*]] = copy_value [[X_ZERO_INIT]]
// CHECK:   [[Y_ZERO_INIT_COPY:%.*]] = copy_value [[Y_ZERO_INIT]]
// CHECK:   [[ZERO_INIT:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]([[X_ZERO_INIT_COPY]], [[Y_ZERO_INIT_COPY]])
// CHECK:   return [[ZERO_INIT]] : $@callee_guaranteed () -> SelfTangentVector
// CHECK: }

// CHECK-LABEL: // CustomTangentVector.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}19CustomTangentVectorV0bgH11Initializer0gH0Qzycvg : $@convention(method) <T, U where T : Differentiable, U : Differentiable> (@in_guaranteed CustomTangentVector<T, U>) -> @owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T.TangentVector> {
// CHECK: bb0([[SELF:%.*]] : $*CustomTangentVector<T, U>):
// CHECK:   // function_ref closure #3 in CustomTangentVector.zeroTangentVectorInitializer.getter
// CHECK:   [[CLOSURE_FN:%.*]] = function_ref @${{.*}}19CustomTangentVectorV0bgH11Initializer0gH0QzycvgAFycfU1_ : $@convention(thin) <τ_0_0, τ_0_1 where τ_0_0 : Differentiable, τ_0_1 : Differentiable> () -> @out τ_0_0.TangentVector
// CHECK: }

// CHECK-LABEL: // closure #1 in MemberwiseTangentVector.zeroTangentVectorInitializer.getter
// CHECK-LABEL: sil private [ossa] @${{.*}}23MemberwiseTangentVectorV0bgH11InitializerAC0gH0VycvgAFycfU_ : $@convention(thin) (@guaranteed @callee_guaranteed () -> Float, @guaranteed @callee_guaranteed () -> Double) -> MemberwiseTangentVector.TangentVector {
// CHECK:   // function_ref MemberwiseTangentVector.TangentVector.init(x:y:)
// CHECK-NOT:   witness_method {{.*}}, #AdditiveArithmetic.zero!getter
// CHECK: }

// CHECK-LABEL: // closure #2 in SelfTangentVector.zeroTangentVectorInitializer.getter
// CHECK-LABEL: sil private [ossa] @${{.*}}17SelfTangentVectorV0bgH11InitializerACycvgACycfU0_ : $@convention(thin) (@guaranteed @callee_guaranteed () -> Float, @guaranteed @callee_guaranteed () -> Double) -> SelfTangentVector {
// CHECK:   // function_ref SelfTangentVector.init(x:y:)
// CHECK-NOT:   witness_method {{.*}}, #AdditiveArithmetic.zero!getter
// CHECK: }

// CHECK-LABEL: // closure #3 in CustomTangentVector.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}19CustomTangentVectorV0bgH11Initializer0gH0QzycvgAFycfU1_ : $@convention(thin) <T, U where T : Differentiable, U : Differentiable> () -> @out T.TangentVector {
// CHECK:  witness_method $T.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK: }
