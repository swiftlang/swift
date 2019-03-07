// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

enum PartialApplyEnumPayload<T, U> {
  case left(T)
  case both(T, U)

  case leftWithLabel(left: T)
  case bothWithLabel(left: T, right: U)

  case tupleWithLabel(both: (T, U))

  // Note: SILGen can emit these thunks correctly, but we disabled
  // the feature since calling the constructor directly (without a
  // thunk) doesn't work yet.
  /* case variadic(_: Int...)
  case variadicWithLabel(indices: Int...)
  case variadicTuple(_: (Int, Int)...)
  case variadicWithOther(String, _: Int...) */

  case autoclosure(@autoclosure () -> ())
}

struct S {}
struct C {}

func partialApplyEnumCases(_ x: S, y: C) {
  _ = PartialApplyEnumPayload<S, C>.left
  _ = PartialApplyEnumPayload<S, C>.both
  _ = PartialApplyEnumPayload<S, C>.leftWithLabel
  _ = PartialApplyEnumPayload<S, C>.tupleWithLabel
  /* _ = PartialApplyEnumPayload<S, C>.variadic
  _ = PartialApplyEnumPayload<S, C>.variadicWithLabel
  _ = PartialApplyEnumPayload<S, C>.variadicTuple
  _ = PartialApplyEnumPayload<S, C>.variadicWithOther */
  _ = PartialApplyEnumPayload<S, C>.autoclosure
}

// CHECK-LABEL: sil shared [transparent] [thunk] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4leftyACyxq_GxcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4leftyACyxq_GxcAEmr0_lF : $@convention(method) <T, U> (@in T, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4bothyACyxq_Gx_q_tcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4bothyACyxq_Gx_q_tcAEmr0_lF : $@convention(method) <T, U> (@in T, @in U, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO13leftWithLabelyACyxq_Gx_tcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO13leftWithLabelyACyxq_Gx_tcAEmr0_lF : $@convention(method) <T, U> (@in T, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO14tupleWithLabelyACyxq_Gx_q_t_tcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO14tupleWithLabelyACyxq_Gx_q_t_tcAEmr0_lF : $@convention(method) <T, U> (@in T, @in U, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO11autoclosureyACyxq_GyyXAcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@guaranteed @callee_guaranteed () -> ()) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] [ossa] @$s17enum_curry_thunks23PartialApplyEnumPayloadO11autoclosureyACyxq_GyyXAcAEmr0_lF : $@convention(method) <T, U> (@owned @callee_guaranteed () -> (), @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {
