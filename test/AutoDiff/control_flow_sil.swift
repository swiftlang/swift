// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s -check-prefix=CHECK-DATA-STRUCTURES
// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -sil-print-after=differentiation -o /dev/null 2>&1 %s | %FileCheck %s -check-prefix=CHECK-SIL

// TODO: Add FileCheck tests.

//===----------------------------------------------------------------------===//
// Conditionals
//===----------------------------------------------------------------------===//

@differentiable
@_silgen_name("cond")
func cond(_ x: Float) -> Float {
  if x > 0 {
    return x + x
  }
  return x - x
}

// CHECK-DATA-STRUCTURES: struct _AD__cond_bb0__PB__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb1__PB__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var predecessor: _AD__cond_bb1__Pred__src_0_wrt_0 { get set }
// CHECK-DATA-STRUCTURES:   @_hasStorage var pullback_0: (Float) -> (Float, Float) { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb2__PB__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var predecessor: _AD__cond_bb2__Pred__src_0_wrt_0 { get set }
// CHECK-DATA-STRUCTURES:   @_hasStorage var pullback_1: (Float) -> (Float, Float) { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb3__PB__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   @_hasStorage var predecessor: _AD__cond_bb3__Pred__src_0_wrt_0 { get set }
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__cond_bb0__Pred__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__cond_bb1__Pred__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   case bb0(_AD__cond_bb0__PB__src_0_wrt_0)
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__cond_bb2__Pred__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   case bb0(_AD__cond_bb0__PB__src_0_wrt_0)
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: enum _AD__cond_bb3__Pred__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   case bb2(_AD__cond_bb2__PB__src_0_wrt_0)
// CHECK-DATA-STRUCTURES:   case bb1(_AD__cond_bb1__PB__src_0_wrt_0)
// CHECK-DATA-STRUCTURES: }

// CHECK-SIL-LABEL: sil hidden @AD__cond__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// CHECK-SIL: bb0([[INPUT_ARG:%.*]] : $Float):
// CHECK-SIL:   [[BB0_PB_STRUCT:%.*]] = struct $_AD__cond_bb0__PB__src_0_wrt_0 ()
// CHECK-SIL:   [[BB1_PRED:%.*]] = enum $_AD__cond_bb1__Pred__src_0_wrt_0, #_AD__cond_bb1__Pred__src_0_wrt_0.bb0!enumelt.1, [[BB0_PB_STRUCT]]
// CHECK-SIL:   [[BB2_PRED:%.*]] = enum $_AD__cond_bb2__Pred__src_0_wrt_0, #_AD__cond_bb2__Pred__src_0_wrt_0.bb0!enumelt.1, [[BB0_PB_STRUCT]]
// CHECK-SIL:   cond_br {{%.*}}, bb1([[BB1_PRED]] : $_AD__cond_bb1__Pred__src_0_wrt_0), bb2([[BB2_PRED]] : $_AD__cond_bb2__Pred__src_0_wrt_

// CHECK-SIL: bb1([[BB1_PRED_ARG:%.*]] : $_AD__cond_bb1__Pred__src_0_wrt_0)
// CHECK-SIL:   [[BB1_PB_STRUCT:%.*]] = struct $_AD__cond_bb1__PB__src_0_wrt_0
// CHECK-SIL:   [[BB3_PRED_PRED1:%.*]] = enum $_AD__cond_bb3__Pred__src_0_wrt_0, #_AD__cond_bb3__Pred__src_0_wrt_0.bb1!enumelt.1, [[BB1_PB_STRUCT]]
// CHECK-SIL:   br bb3({{.*}} : $Float, [[BB3_PRED_PRED1]] : $_AD__cond_bb3__Pred__src_0_wrt_0)

// CHECK-SIL: bb2([[BB2_PRED_ARG:%.*]] : $_AD__cond_bb2__Pred__src_0_wrt_0)
// CHECK-SIL:   [[BB2_PB_STRUCT:%.*]] = struct $_AD__cond_bb2__PB__src_0_wrt_0
// CHECK-SIL:   [[BB3_PRED_PRED2:%.*]] = enum $_AD__cond_bb3__Pred__src_0_wrt_0, #_AD__cond_bb3__Pred__src_0_wrt_0.bb2!enumelt.1, [[BB2_PB_STRUCT]]
// CHECK-SIL:   br bb3({{.*}} : $Float, [[BB3_PRED_PRED2]] : $_AD__cond_bb3__Pred__src_0_wrt_0)

// CHECK-SIL: bb3([[ORIG_RES:%.*]] : $Float, [[BB3_PRED_ARG:%.*]] : $_AD__cond_bb3__Pred__src_0_wrt_0)
// CHECK-SIL:   [[BB3_PB_STRUCT:%.*]] = struct $_AD__cond_bb3__PB__src_0_wrt_0
// CHECK-SIL:   [[ADJOINT_REF:%.*]] = function_ref @AD__cond__adjoint_src_0_wrt_0
// CHECK-SIL:   [[PB:%.*]] = partial_apply [callee_guaranteed] [[ADJOINT_REF]]([[BB3_PB_STRUCT]])
// CHECK-SIL:   [[VJP_RESULT:%.*]] = tuple ([[ORIG_RES]] : $Float, [[PB]] : $@callee_guaranteed (Float) -> Float)
// CHECK-SIL:   return [[VJP_RESULT]]

// CHECK-SIL-LABEL: sil hidden @AD__cond__adjoint_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed _AD__cond_bb3__PB__src_0_wrt_0) -> Float {
// CHECK-SIL: bb0([[SEED:%.*]] : $Float, [[BB3_PB_STRUCT:%.*]] : $_AD__cond_bb3__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB3_PRED:%.*]] = struct_extract %1 : $_AD__cond_bb3__PB__src_0_wrt_0, #_AD__cond_bb3__PB__src_0_wrt_0.predecessor
// CHECK-SIL:   switch_enum [[BB3_PRED]] : $_AD__cond_bb3__Pred__src_0_wrt_0, case #_AD__cond_bb3__Pred__src_0_wrt_0.bb2!enumelt.1: bb3, case #_AD__cond_bb3__Pred__src_0_wrt_0.bb1!enumelt.1: bb1

// CHECK-SIL: bb1([[BB3_PRED1_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   br bb2({{%.*}} : $Float, {{%.*}}: $Float, [[BB3_PRED1_TRAMP_PB_STRUCT]] : $_AD__cond_bb1__PB__src_0_wrt_0)

// CHECK-SIL: bb2({{%.*}} : $Float, {{%.*}} : $Float, [[BB1_PB_STRUCT:%.*]] : $_AD__cond_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB1_PB:%.*]] = struct_extract [[BB1_PB_STRUCT]]
// CHECK-SIL:   [[BB1_ADJVALS:%.*]] = apply [[BB1_PB]]([[SEED]]) : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK-SIL:   [[BB1_PRED:%.*]] = struct_extract [[BB1_PB_STRUCT]]
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   [[BB1_PB_STRUCT_DATA:%.*]] = unchecked_enum_data [[BB1_PRED]]
// CHECK-SIL:   br bb5([[BB1_PB_STRUCT_DATA]] : $_AD__cond_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb3([[BB3_PRED2_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   br bb4({{%.*}} : $Float, {{%.*}}: $Float, [[BB3_PRED2_TRAMP_PB_STRUCT]] : $_AD__cond_bb2__PB__src_0_wrt_0)

// CHECK-SIL: bb4({{%.*}} : $Float, {{%.*}} : $Float, [[BB2_PB_STRUCT:%.*]] : $_AD__cond_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB2_PB:%.*]] = struct_extract [[BB2_PB_STRUCT]]
// CHECK-SIL:   [[BB2_ADJVALS:%.*]] = apply [[BB2_PB]]([[SEED]]) : $@callee_guaranteed (Float) -> (Float, Float)
// CHECK-SIL:   [[BB2_PRED:%.*]] = struct_extract [[BB2_PB_STRUCT]]
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   [[BB2_PB_STRUCT_DATA:%.*]] = unchecked_enum_data [[BB2_PRED]]
// CHECK-SIL:   br bb6([[BB2_PB_STRUCT_DATA]] : $_AD__cond_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb5([[BB1_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   br bb7({{%.*}} : $Float, [[BB1_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb6([[BB2_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   br bb7({{%.*}} : $Float, [[BB2_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb7({{%.*}} : $Float, [[BB0_PB_STRUCT:%.*]] : $_AD__cond_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   release_value {{%.*}} : $Float
// CHECK-SIL:   return {{%.*}} : $Float

@differentiable
@_silgen_name("nested_cond")
func nested_cond(_ x: Float, _ y: Float) -> Float {
  if x > 0 {
    if y > 10 {
      return x * y
    } else {
      return x + y
    }
  }
  return y - x
}

@differentiable
@_silgen_name("nested_cond_generic")
func nested_cond_generic<T : Differentiable & FloatingPoint>(_ x: T, _ y: T) -> T {
  if x > 0 {
    if y > 10 {
      return y
    } else {
      return x
    }
  }
  return y
}

@differentiable
@_silgen_name("loop_generic")
func loop_generic<T : Differentiable & FloatingPoint>(_ x: T) -> T {
  var result = x
  for _ in 1..<3 {
    var y = x
    for _ in 1..<3 {
      result = y
      y = result
    }
  }
  return result
}

// Test control flow + tuple buffer.
// Verify that adjoint buffers are not allocated for address projections.

@differentiable
@_silgen_name("cond_tuple_var")
func cond_tuple_var(_ x: Float) -> Float {
  // expected-warning @+1 {{variable 'y' was never mutated; consider changing to 'let' constant}}
  var y = (x, x)
  if x > 0 {
    return y.0
  }
  return y.1
}
// CHECK-SIL-LABEL: sil hidden @AD__cond_tuple_var__adjoint_src_0_wrt_0 : $@convention(thin) (Float, @guaranteed _AD__cond_tuple_var_bb3__PB__src_0_wrt_0) -> Float {
// CHECK-SIL: bb0([[SEED:%.*]] : $Float, [[BB3_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb3__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB3_PRED:%.*]] = struct_extract %1 : $_AD__cond_tuple_var_bb3__PB__src_0_wrt_0, #_AD__cond_tuple_var_bb3__PB__src_0_wrt_0.predecessor
// CHECK-SIL:   copy_addr {{%.*}} to {{%.*}} : $*(Float, Float)
// CHECK-SIL-NOT:   copy_addr {{%.*}} to {{%.*}} : $*Float
// CHECK-SIL:   switch_enum [[BB3_PRED]] : $_AD__cond_tuple_var_bb3__Pred__src_0_wrt_0, case #_AD__cond_tuple_var_bb3__Pred__src_0_wrt_0.bb2!enumelt.1: bb3, case #_AD__cond_tuple_var_bb3__Pred__src_0_wrt_0.bb1!enumelt.1: bb1

// CHECK-SIL: bb1([[BB3_PRED1_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   br bb2({{%.*}} : $Float, {{%.*}} : $Float, [[BB3_PRED1_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb1__PB__src_0_wrt_0)

// CHECK-SIL: bb2({{%.*}} : $Float, {{%.*}} : $Float, [[BB1_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB1_PRED:%.*]] = struct_extract [[BB1_PB_STRUCT]]
// CHECK-SIL:   copy_addr {{%.*}} to {{%.*}} : $*(Float, Float)
// CHECK-SIL-NOT:   copy_addr {{%.*}} to {{%.*}} : $*Float
// CHECK-SIL:   [[BB1_PB_STRUCT_DATA:%.*]] = unchecked_enum_data [[BB1_PRED]]
// CHECK-SIL:   br bb5([[BB1_PB_STRUCT_DATA]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb3([[BB3_PRED2_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   br bb4({{%.*}} : $Float, {{%.*}} : $Float, [[BB3_PRED2_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb2__PB__src_0_wrt_0)

// CHECK-SIL: bb4({{%.*}} : $Float, {{%.*}} : $Float, [[BB2_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB2_PRED:%.*]] = struct_extract [[BB2_PB_STRUCT]]
// CHECK-SIL:   copy_addr {{%.*}} to {{%.*}} : $*(Float, Float)
// CHECK-SIL-NOT:   copy_addr {{%.*}} to {{%.*}} : $*Float
// CHECK-SIL:   [[BB2_PB_STRUCT_DATA:%.*]] = unchecked_enum_data [[BB2_PRED]]
// CHECK-SIL:   br bb6([[BB2_PB_STRUCT_DATA]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb5([[BB1_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   br bb7({{%.*}} : $Float, [[BB1_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb6([[BB2_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   br bb7({{%.*}} : $Float, [[BB2_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb7({{%.*}} : $Float, [[BB0_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   return {{%.*}} : $Float
