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

// CHECK-LABEL: sil private [ossa] @$s17enum_curry_thunks21partialApplyEnumCases_1yyAA1SV_AA1CVtFAA07PartialeF7PayloadOyAeGGAEcAJmcfu_AjEcfu0_ : $@convention(thin) (S, @thin PartialApplyEnumPayload<S, C>.Type) -> @owned PartialApplyEnumPayload<S, C> {

// CHECK-LABEL: sil private [ossa] @$s17enum_curry_thunks21partialApplyEnumCases_1yyAA1SV_AA1CVtFAA07PartialeF7PayloadOyAeGGAE_AGtcAJmcfu1_AjE_AGtcfu2_ : $@convention(thin) (S, C, @thin PartialApplyEnumPayload<S, C>.Type) -> @owned PartialApplyEnumPayload<S, C> {

// CHECK-LABEL: sil private [ossa] @$s17enum_curry_thunks21partialApplyEnumCases_1yyAA1SV_AA1CVtFAA07PartialeF7PayloadOyAeGGAEcAJmcfu3_AjEcfu4_ : $@convention(thin) (S, @thin PartialApplyEnumPayload<S, C>.Type) -> @owned PartialApplyEnumPayload<S, C> {

// CHECK-LABEL: sil private [ossa] @$s17enum_curry_thunks21partialApplyEnumCases_1yyAA1SV_AA1CVtFAA07PartialeF7PayloadOyAeGGAE_AGt_tcAJmcfu5_AjE_AGt_tcfu6_ : $@convention(thin) (S, C, @thin PartialApplyEnumPayload<S, C>.Type) -> @owned PartialApplyEnumPayload<S, C> {

// CHECK-LABEL: sil private [ossa] @$s17enum_curry_thunks21partialApplyEnumCases_1yyAA1SV_AA1CVtFAA07PartialeF7PayloadOyAeGGyyXAcAJmcfu7_AJyyXAcfu8_ : $@convention(thin) (@guaranteed @callee_guaranteed () -> (), @thin PartialApplyEnumPayload<S, C>.Type) -> @owned PartialApplyEnumPayload<S, C> {
