// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -differentiation-disable-adjoint-gen %s | %FileCheck %s -check-prefix=CHECK-DATA-STRUCTURES
// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -differentiation-disable-adjoint-gen %s | %FileCheck %s -check-prefix=CHECK-SIL

// Test cond. A simple if-diamond.

@differentiable
@_silgen_name("cond")
func cond(_ x: Float) -> Float {
  if x > 0 {
    return x + x
  }
  return x - x
}

// CHECK-DATA-STRUCTURES: enum _AD__cond_bb0__Enum__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb0__Struct__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__cond_bb1__Enum__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   case bb0(_AD__cond_bb0__Struct__src_0_wrt_0)
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb1__Struct__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var predecessor: _AD__cond_bb1__Enum__src_0_wrt_0 { get set }
// CHECK-DATA-STRUCTURES:   @_hasStorage var pullback_0: (Float) -> (Float, Float) { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__cond_bb2__Enum__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   case bb0(_AD__cond_bb0__Struct__src_0_wrt_0)
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb2__Struct__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var predecessor: _AD__cond_bb2__Enum__src_0_wrt_0 { get set }
// CHECK-DATA-STRUCTURES:   @_hasStorage var pullback_1: (Float) -> (Float, Float) { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__cond_bb3__Enum__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   case bb2(_AD__cond_bb2__Struct__src_0_wrt_0)
// CHECK-DATA-STRUCTURES:   case bb1(_AD__cond_bb1__Struct__src_0_wrt_0)
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb3__Struct__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var predecessor: _AD__cond_bb3__Enum__src_0_wrt_0 { get set }
// CHECK-DATA-STRUCTURES: }

// CHECK-SIL-LABEL: sil hidden @AD__cond__vjp_src_0_wrt_0
// CHECK-SIL: bb0([[INPUT_ARG:%.*]] : $Float):
// CHECK-SIL:   [[BB0_STRUCT:%.*]] = struct $_AD__cond_bb0__Struct__src_0_wrt_0 ()
// CHECK-SIL:   [[BB1_ENUM:%.*]] = enum $_AD__cond_bb1__Enum__src_0_wrt_0, #_AD__cond_bb1__Enum__src_0_wrt_0.bb0!enumelt.1, [[BB0_STRUCT]]
// CHECK-SIL:   [[BB2_ENUM:%.*]] = enum $_AD__cond_bb2__Enum__src_0_wrt_0, #_AD__cond_bb2__Enum__src_0_wrt_0.bb0!enumelt.1, [[BB0_STRUCT]]
// CHECK-SIL:   cond_br {{%.*}}, bb1([[BB1_ENUM]] : $_AD__cond_bb1__Enum__src_0_wrt_0), bb2([[BB2_ENUM]] : $_AD__cond_bb2__Enum__src_0_wrt_

// CHECK-SIL: bb1([[BB1_ENUM_ARG:%.*]] : $_AD__cond_bb1__Enum__src_0_wrt_0)
// CHECK-SIL:   [[BB1_STRUCT:%.*]] = struct $_AD__cond_bb1__Struct__src_0_wrt_0
// CHECK-SIL:   [[BB3_ENUM_PRED1:%.*]] = enum $_AD__cond_bb3__Enum__src_0_wrt_0, #_AD__cond_bb3__Enum__src_0_wrt_0.bb1!enumelt.1, [[BB1_STRUCT]]
// CHECK-SIL:   br bb3({{.*}} : $Float, [[BB3_ENUM_PRED1]] : $_AD__cond_bb3__Enum__src_0_wrt_0)

// CHECK-SIL: bb2([[BB2_ENUM_ARG:%.*]] : $_AD__cond_bb2__Enum__src_0_wrt_0)
// CHECK-SIL:   [[BB2_STRUCT:%.*]] = struct $_AD__cond_bb2__Struct__src_0_wrt_0
// CHECK-SIL:   [[BB3_ENUM_PRED2:%.*]] = enum $_AD__cond_bb3__Enum__src_0_wrt_0, #_AD__cond_bb3__Enum__src_0_wrt_0.bb2!enumelt.1, [[BB2_STRUCT]]
// CHECK-SIL:   br bb3({{.*}} : $Float, [[BB3_ENUM_PRED2]] : $_AD__cond_bb3__Enum__src_0_wrt_0)

// CHECK-SIL: bb3([[ORIG_RES:%.*]] : $Float, [[BB3_ENUM_ARG:%.*]] : $_AD__cond_bb3__Enum__src_0_wrt_0)
// CHECK-SIL:   [[BB3_STRUCT:%.*]] = struct $_AD__cond_bb3__Struct__src_0_wrt_0
// CHECK-SIL:   [[ADJOINT_REF:%.*]] = function_ref @AD__cond__adjoint_src_0_wrt_0
// CHECK-SIL:   [[PB:%.*]] = partial_apply [callee_guaranteed] [[ADJOINT_REF]]([[BB3_STRUCT]])
// CHECK-SIL:   [[VJP_RESULT:%.*]] = tuple ([[ORIG_RES]] : $Float, [[PB]] : $@callee_guaranteed (Float) -> Float)
// CHECK-SIL:   return [[VJP_RESULT]]
// Test cond.

@differentiable
@_silgen_name("nested_conds")
func nestedConds<T : Differentiable & FloatingPoint>(_ x: T, _ y: T) -> T {
  if x > 0 {
    if y > 10 {
      return y
    } else {
      return x
    }
  }
  return y
}

// TODO: Add FileCheck tests.
