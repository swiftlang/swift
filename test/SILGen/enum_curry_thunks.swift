// RUN: %target-swift-emit-silgen -parse-as-library %s | %FileCheck %s

enum PartialApplyEnumPayload<T, U> {
  case Left(T)
  case Both(T, U)

  case LeftWithLabel(left: T)
  case BothWithLabel(left: T, right: U)

  case TupleWithLabel(both: (T, U))

  // Note: SILGen can emit these thunks correctly, but we disabled
  // the feature since calling the constructor directly (without a
  // thunk) doesn't work yet.
  /* case Variadic(_: Int...)
  case VariadicWithLabel(indices: Int...)
  case VariadicTuple(_: (Int, Int)...)
  case VariadicWithOther(String, _: Int...) */

  case Autoclosure(@autoclosure () -> ())
}

struct S {}
struct C {}

func partialApplyEnumCases(_ x: S, y: C) {
  _ = PartialApplyEnumPayload<S, C>.Left
  _ = PartialApplyEnumPayload<S, C>.Both
  _ = PartialApplyEnumPayload<S, C>.LeftWithLabel
  _ = PartialApplyEnumPayload<S, C>.TupleWithLabel
  /* _ = PartialApplyEnumPayload<S, C>.Variadic
  _ = PartialApplyEnumPayload<S, C>.VariadicWithLabel
  _ = PartialApplyEnumPayload<S, C>.VariadicTuple
  _ = PartialApplyEnumPayload<S, C>.VariadicWithOther */
  _ = PartialApplyEnumPayload<S, C>.Autoclosure
}

// CHECK-LABEL: sil shared [transparent] [thunk] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4LeftyACyxq_GxcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4LeftyACyxq_GxcAEmr0_lF : $@convention(method) <T, U> (@in T, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4BothyACyxq_Gx_q_tcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] @$s17enum_curry_thunks23PartialApplyEnumPayloadO4BothyACyxq_Gx_q_tcAEmr0_lF : $@convention(method) <T, U> (@in T, @in U, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] @$s17enum_curry_thunks23PartialApplyEnumPayloadO13LeftWithLabelyACyxq_Gx_tcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] @$s17enum_curry_thunks23PartialApplyEnumPayloadO13LeftWithLabelyACyxq_Gx_tcAEmr0_lF : $@convention(method) <T, U> (@in T, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] @$s17enum_curry_thunks23PartialApplyEnumPayloadO14TupleWithLabelyACyxq_Gx_q_t_tcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@in_guaranteed T, @in_guaranteed U) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] @$s17enum_curry_thunks23PartialApplyEnumPayloadO14TupleWithLabelyACyxq_Gx_q_t_tcAEmr0_lF : $@convention(method) <T, U> (@in T, @in U, @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {

// CHECK-LABEL: sil shared [transparent] [thunk] @$s17enum_curry_thunks23PartialApplyEnumPayloadO11AutoclosureyACyxq_GyyXAcAEmr0_lFTc : $@convention(thin) <T, U> (@thin PartialApplyEnumPayload<T, U>.Type) -> @owned @callee_guaranteed (@guaranteed @callee_guaranteed () -> ()) -> @out PartialApplyEnumPayload<T, U> {
// CHECK-LABEL: sil shared [transparent] @$s17enum_curry_thunks23PartialApplyEnumPayloadO11AutoclosureyACyxq_GyyXAcAEmr0_lF : $@convention(method) <T, U> (@owned @callee_guaranteed () -> (), @thin PartialApplyEnumPayload<T, U>.Type) -> @out PartialApplyEnumPayload<T, U> {
