// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// Check `Differentiable.zeroTangentVectorInitializer` derivation.
//
// There are two cases:
// 1. Memberwise derivation.
//
//     var zeroTangentVectorInitializer: () -> TangentVector {
//       { [xZeroTanInit = x.zeroTangentVectorInitializer,
//          yZeroTanInit = y.zeroTangentVectorInitializer, ...] in
//         return TangentVector(x: xZeroTanInit(), y: yZeroTanInit(), ...)
//       }
//     }
//
// 2. `{ TangentVector.zero }` fallback derivation.
//
//     var zeroTangentVectorInitializer: () -> TangentVector {
//       { TangentVector.zero }
//     }

import _Differentiation

// - MARK: Structs

struct MemberwiseTangentVectorStruct: Differentiable {
  var x: Float
  var y: Double

  // Expected memberwise `zeroTangentVectorInitializer` synthesis (1).
}

struct SelfTangentVectorStruct: Differentiable & AdditiveArithmetic {
  var x: Float
  var y: Double
  typealias TangentVector = Self

  // Expected memberwise `zeroTangentVectorInitializer` synthesis (1).
}

struct CustomTangentVectorStruct<T: Differentiable, U: Differentiable>: Differentiable {
  var x: T
  var y: U

  typealias TangentVector = T.TangentVector
  mutating func move(along direction: TangentVector) {}

  // Expected fallback `zeroTangentVectorInitializer` synthesis (2).
}

// - MARK: Classes

class MemberwiseTangentVectorClass: Differentiable {
  var x: Float = 0.0
  var y: Double = 0.0

  // Expected memberwise `zeroTangentVectorInitializer` synthesis (1).
}

final class SelfTangentVectorClass: Differentiable & AdditiveArithmetic {
  var x: Float = 0.0
  var y: Double = 0.0
  typealias TangentVector = SelfTangentVectorClass

  static func ==(lhs: SelfTangentVectorClass, rhs: SelfTangentVectorClass) -> Bool { fatalError() }
  static var zero: Self { fatalError() }
  static func +(lhs: SelfTangentVectorClass, rhs: SelfTangentVectorClass) -> Self { fatalError() }
  static func -(lhs: SelfTangentVectorClass, rhs: SelfTangentVectorClass) -> Self { fatalError() }

  // Expected memberwise `zeroTangentVectorInitializer` synthesis (1).
}

class CustomTangentVectorClass<T: Differentiable, U: Differentiable>: Differentiable {
  var x: T
  var y: U

  init(x: T, y: U) {
    self.x = x
    self.y = y
  }

  typealias TangentVector = T.TangentVector
  func move(along direction: TangentVector) {}

  // Expected fallback `zeroTangentVectorInitializer` synthesis (2).
}

// - MARK: Enums

enum SelfTangentVectorEnum: Differentiable & AdditiveArithmetic {
  case a([Float])
  case b([Float], Float)
  case c

  typealias TangentVector = SelfTangentVectorEnum

  static func ==(lhs: Self, rhs: Self) -> Bool { fatalError() }
  static var zero: Self { fatalError() }
  static func +(lhs: Self, rhs: Self) -> Self { fatalError() }
  static func -(lhs: Self, rhs: Self) -> Self { fatalError() }

  // TODO(TF-1012): Implement memberwise `zeroTangentVectorInitializer` synthesis for enums.
  // Expected fallback `zeroTangentVectorInitializer` synthesis (2).
}

enum CustomTangentVectorEnum<T: Differentiable>: Differentiable {
  case a(T)

  typealias TangentVector = T.TangentVector
  mutating func move(along direction: TangentVector) {}

  // Expected fallback `zeroTangentVectorInitializer` synthesis (2).
}

// CHECK-LABEL: // MemberwiseTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}MemberwiseTangentVectorStructV0bgH11InitializerAC0gH0Vycvg : $@convention(method) (MemberwiseTangentVectorStruct) -> @owned @callee_guaranteed () -> MemberwiseTangentVectorStruct.TangentVector {
// CHECK: bb0([[SELF:%.*]] : $MemberwiseTangentVectorStruct):
// CHECK:   [[X_PROP:%.*]] = struct_extract [[SELF]] : $MemberwiseTangentVectorStruct, #MemberwiseTangentVectorStruct.x
// CHECK:   [[X_ZERO_INIT_FN:%.*]] = function_ref @$sSf{{.*}}E28zeroTangentVectorInitializerSfycvg : $@convention(method) (Float) -> @owned @callee_guaranteed () -> Float
// CHECK:   [[X_ZERO_INIT:%.*]] = apply [[X_ZERO_INIT_FN]]([[X_PROP]])
// CHECK:   [[Y_PROP:%.*]] = struct_extract [[SELF]] : $MemberwiseTangentVectorStruct, #MemberwiseTangentVectorStruct.y
// CHECK:   [[Y_ZERO_INIT_FN:%.*]] = function_ref @$sSd{{.*}}E28zeroTangentVectorInitializerSdycvg : $@convention(method) (Double) -> @owned @callee_guaranteed () -> Double
// CHECK:   [[Y_ZERO_INIT:%.*]] = apply [[Y_ZERO_INIT_FN]]([[Y_PROP]])
// CHECK:   // function_ref closure #1 in MemberwiseTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK:   [[CLOSURE_FN:%.*]] = function_ref @${{.*}}MemberwiseTangentVectorStructV0bgH11InitializerAC0gH0VycvgAFycfU_
// CHECK:   [[X_ZERO_INIT_COPY:%.*]] = copy_value [[X_ZERO_INIT]]
// CHECK:   [[Y_ZERO_INIT_COPY:%.*]] = copy_value [[Y_ZERO_INIT]]
// CHECK:   [[ZERO_INIT:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]([[X_ZERO_INIT_COPY]], [[Y_ZERO_INIT_COPY]])
// CHECK:   return [[ZERO_INIT]] : $@callee_guaranteed () -> MemberwiseTangentVectorStruct.TangentVector
// CHECK: }

// CHECK-LABEL: // SelfTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}SelfTangentVectorStructV0bgH11InitializerACycvg : $@convention(method) (SelfTangentVectorStruct) -> @owned @callee_guaranteed () -> SelfTangentVectorStruct {
// CHECK: bb0([[SELF:%.*]] : $SelfTangentVectorStruct):
// CHECK:   [[X_PROP:%.*]] = struct_extract [[SELF]] : $SelfTangentVectorStruct, #SelfTangentVectorStruct.x
// CHECK:   [[X_ZERO_INIT_FN:%.*]] = function_ref @$sSf{{.*}}E28zeroTangentVectorInitializerSfycvg : $@convention(method) (Float) -> @owned @callee_guaranteed () -> Float
// CHECK:   [[X_ZERO_INIT:%.*]] = apply [[X_ZERO_INIT_FN]]([[X_PROP]])
// CHECK:   [[Y_PROP:%.*]] = struct_extract [[SELF]] : $SelfTangentVectorStruct, #SelfTangentVectorStruct.y
// CHECK:   [[Y_ZERO_INIT_FN:%.*]] = function_ref @$sSd{{.*}}E28zeroTangentVectorInitializerSdycvg : $@convention(method) (Double) -> @owned @callee_guaranteed () -> Double
// CHECK:   [[Y_ZERO_INIT:%.*]] = apply [[Y_ZERO_INIT_FN]]([[Y_PROP]])
// CHECK:   // function_ref closure #1 in SelfTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK:   [[CLOSURE_FN:%.*]] = function_ref @${{.*}}SelfTangentVectorStructV0bgH11InitializerACycvgACycfU_
// CHECK:   [[X_ZERO_INIT_COPY:%.*]] = copy_value [[X_ZERO_INIT]]
// CHECK:   [[Y_ZERO_INIT_COPY:%.*]] = copy_value [[Y_ZERO_INIT]]
// CHECK:   [[ZERO_INIT:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]([[X_ZERO_INIT_COPY]], [[Y_ZERO_INIT_COPY]])
// CHECK:   return [[ZERO_INIT]] : $@callee_guaranteed () -> SelfTangentVectorStruct
// CHECK: }

// CHECK-LABEL: // CustomTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}CustomTangentVectorStructV0bgH11Initializer0gH0Qzycvg : $@convention(method) <T, U where T : Differentiable, U : Differentiable> (@in_guaranteed CustomTangentVectorStruct<T, U>) -> @owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T.TangentVector> {
// CHECK: bb0([[SELF:%.*]] : $*CustomTangentVectorStruct<T, U>):
// CHECK:   // function_ref closure #1 in CustomTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK:   function_ref @${{.*}}CustomTangentVectorStructV0bgH11Initializer0gH0QzycvgAFycfU_
// CHECK: }

// CHECK-LABEL: // MemberwiseTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}MemberwiseTangentVectorClassC0bgH11InitializerAC0gH0Vycvg : $@convention(method) (@guaranteed MemberwiseTangentVectorClass) -> @owned @callee_guaranteed () -> MemberwiseTangentVectorClass.TangentVector {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $MemberwiseTangentVectorClass):
// CHECK:   [[X_PROP_METHOD:%.*]] = class_method [[SELF]] : $MemberwiseTangentVectorClass, #MemberwiseTangentVectorClass.x!getter
// CHECK:   [[X_PROP:%.*]] = apply [[X_PROP_METHOD]]([[SELF]])
// CHECK:   [[X_ZERO_INIT_FN:%.*]] = function_ref @$sSf{{.*}}E28zeroTangentVectorInitializerSfycvg : $@convention(method) (Float) -> @owned @callee_guaranteed () -> Float
// CHECK:   [[X_ZERO_INIT:%.*]] = apply [[X_ZERO_INIT_FN]]([[X_PROP]])
// CHECK:   [[Y_PROP_METHOD:%.*]] = class_method [[SELF]] : $MemberwiseTangentVectorClass, #MemberwiseTangentVectorClass.y!getter
// CHECK:   [[Y_PROP:%.*]] = apply [[Y_PROP_METHOD]]([[SELF]])
// CHECK:   [[Y_ZERO_INIT_FN:%.*]] = function_ref @$sSd{{.*}}E28zeroTangentVectorInitializerSdycvg : $@convention(method) (Double) -> @owned @callee_guaranteed () -> Double
// CHECK:   [[Y_ZERO_INIT:%.*]] = apply [[Y_ZERO_INIT_FN]]([[Y_PROP]])
// CHECK:   // function_ref closure #1 in MemberwiseTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK:   [[CLOSURE_FN:%.*]] = function_ref @${{.*}}MemberwiseTangentVectorClassC0bgH11InitializerAC0gH0VycvgAFycfU_
// CHECK:   [[X_ZERO_INIT_COPY:%.*]] = copy_value [[X_ZERO_INIT]]
// CHECK:   [[Y_ZERO_INIT_COPY:%.*]] = copy_value [[Y_ZERO_INIT]]
// CHECK:   [[ZERO_INIT:%.*]] = partial_apply [callee_guaranteed] [[CLOSURE_FN]]([[X_ZERO_INIT_COPY]], [[Y_ZERO_INIT_COPY]])
// CHECK:   return [[ZERO_INIT]] : $@callee_guaranteed () -> MemberwiseTangentVectorClass.TangentVector
// CHECK: }

// CHECK-LABEL: // SelfTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}SelfTangentVectorClassC0bgH11InitializerACycvg : $@convention(method) (@guaranteed SelfTangentVectorClass) -> @owned @callee_guaranteed () -> @owned SelfTangentVectorClass {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $SelfTangentVectorClass):
// CHECK:   // function_ref closure #1 in SelfTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK:   function_ref @${{.*}}SelfTangentVectorClassC0bgH11InitializerACycvgACycfU_
// CHECK: }

// CHECK-LABEL: // CustomTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}CustomTangentVectorClassC0bgH11Initializer0gH0Qzycvg : $@convention(method) <T, U where T : Differentiable, U : Differentiable> (@guaranteed CustomTangentVectorClass<T, U>) -> @owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T.TangentVector> {
// CHECK: bb0(%0 : @guaranteed $CustomTangentVectorClass<T, U>):
// CHECK:   // function_ref closure #1 in CustomTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK:   function_ref @${{.*}}CustomTangentVectorClassC0bgH11Initializer0gH0QzycvgAFycfU_
// CHECK: }

// CHECK-LABEL: // SelfTangentVectorEnum.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}SelfTangentVectorEnumO0bgH11InitializerACycvg : $@convention(method) (@guaranteed SelfTangentVectorEnum) -> @owned @callee_guaranteed () -> @owned SelfTangentVectorEnum {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $SelfTangentVectorEnum):
// CHECK:   // function_ref closure #1 in SelfTangentVectorEnum.zeroTangentVectorInitializer.getter
// CHECK:   function_ref @${{.*}}SelfTangentVectorEnumO0bgH11InitializerACycvgACycfU_
// CHECK: }

// CHECK-LABEL: // CustomTangentVectorEnum.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil hidden [_semantics "autodiff.nonvarying"] [ossa] @${{.*}}CustomTangentVectorEnumO0bgH11Initializer0gH0Qzycvg : $@convention(method) <T where T : Differentiable> (@in_guaranteed CustomTangentVectorEnum<T>) -> @owned @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <T.TangentVector> {
// CHECK: bb0([[SELF:%.*]] : $*CustomTangentVectorEnum<T>):
// CHECK:   // function_ref closure #1 in CustomTangentVectorEnum.zeroTangentVectorInitializer.getter
// CHECK:   function_ref @${{.*}}CustomTangentVectorEnumO0bgH11Initializer0gH0QzycvgAFycfU_
// CHECK: }

// CHECK-LABEL: // closure #1 in MemberwiseTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}MemberwiseTangentVectorStructV0bgH11InitializerAC0gH0VycvgAFycfU_ : $@convention(thin) (@guaranteed @callee_guaranteed () -> Float, @guaranteed @callee_guaranteed () -> Double) -> MemberwiseTangentVectorStruct.TangentVector {
// CHECK:   // function_ref MemberwiseTangentVectorStruct.TangentVector.init(x:y:)
// CHECK-NOT:   // function_ref static {{.*}}.zero.getter
// CHECK-NOT:   witness_method {{.*}}, #AdditiveArithmetic.zero!getter
// CHECK: }

// CHECK-LABEL: // closure #1 in SelfTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}SelfTangentVectorStructV0bgH11InitializerACycvgACycfU_ : $@convention(thin) (@guaranteed @callee_guaranteed () -> Float, @guaranteed @callee_guaranteed () -> Double) -> SelfTangentVectorStruct {
// CHECK:   // function_ref SelfTangentVectorStruct.init(x:y:)
// CHECK-NOT:   // function_ref static {{.*}}.zero.getter
// CHECK-NOT:   witness_method {{.*}}, #AdditiveArithmetic.zero!getter
// CHECK: }

// CHECK-LABEL: // closure #1 in CustomTangentVectorStruct.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}CustomTangentVectorStructV0bgH11Initializer0gH0QzycvgAFycfU_ : $@convention(thin) <T, U where T : Differentiable, U : Differentiable> () -> @out T.TangentVector {
// CHECK:   witness_method $T.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK: }

// CHECK-LABEL: // closure #1 in MemberwiseTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}MemberwiseTangentVectorClassC0bgH11InitializerAC0gH0VycvgAFycfU_ : $@convention(thin) (@guaranteed @callee_guaranteed () -> Float, @guaranteed @callee_guaranteed () -> Double) -> MemberwiseTangentVectorClass.TangentVector {
// CHECK:   // function_ref MemberwiseTangentVectorClass.TangentVector.init(x:y:)
// CHECK-NOT:   // function_ref static {{.*}}.zero.getter
// CHECK-NOT:   witness_method {{.*}}, #AdditiveArithmetic.zero!getter
// CHECK: }

// CHECK-LABEL: // closure #1 in SelfTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}SelfTangentVectorClassC0bgH11InitializerACycvgACycfU_ : $@convention(thin) () -> @owned SelfTangentVectorClass {
// CHECK:   // function_ref static SelfTangentVectorClass.zero.getter
// CHECK:   function_ref @${{.*}}SelfTangentVectorClassC0B0ACXDvgZ : $@convention(method) (@thick SelfTangentVectorClass.Type) -> @owned SelfTangentVectorClass
// CHECK: }

// CHECK-LABEL: // closure #1 in CustomTangentVectorClass.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}CustomTangentVectorClassC0bgH11Initializer0gH0QzycvgAFycfU_ : $@convention(thin) <T, U where T : Differentiable, U : Differentiable> () -> @out T.TangentVector {
// CHECK:   witness_method $T.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK: }

// TODO(TF-1012): Implement memberwise `zeroTangentVectorInitializer` synthesis for enums.
// CHECK-LABEL: // closure #1 in SelfTangentVectorEnum.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @${{.*}}SelfTangentVectorEnumO0bgH11InitializerACycvgACycfU_ : $@convention(thin) () -> @owned SelfTangentVectorEnum {
// CHECK:   // function_ref static SelfTangentVectorEnum.zero.getter
// CHECK:   function_ref @${{.*}}SelfTangentVectorEnumO0B0ACvgZ : $@convention(method) (@thin SelfTangentVectorEnum.Type) -> @owned SelfTangentVectorEnum
// CHECK: }

// CHECK-LABEL: // closure #1 in CustomTangentVectorEnum.zeroTangentVectorInitializer.getter
// CHECK-NEXT: sil private [ossa] @$s39derived_zero_tangent_vector_initializer23CustomTangentVectorEnumO0bgH11Initializer0gH0QzycvgAFycfU_ : $@convention(thin) <T where T : Differentiable> () -> @out T.TangentVector {
// CHECK:   witness_method $T.TangentVector, #AdditiveArithmetic.zero!getter
// CHECK: }
