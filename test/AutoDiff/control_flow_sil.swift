// RUN: %target-swift-frontend -emit-sil -verify -Xllvm -debug-only=differentiation 2>&1 %s | %FileCheck %s -check-prefix=CHECK-DATA-STRUCTURES
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
// CHECK-DATA-STRUCTURES:   var predecessor: _AD__cond_bb1__Pred__src_0_wrt_0
// CHECK-DATA-STRUCTURES:   var pullback_0: (Float) -> (Float, Float)
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb2__PB__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   var predecessor: _AD__cond_bb2__Pred__src_0_wrt_0
// CHECK-DATA-STRUCTURES:   var pullback_1: (Float) -> (Float, Float)
// CHECK-DATA-STRUCTURES: }
// CHECK-DATA-STRUCTURES: struct _AD__cond_bb3__PB__src_0_wrt_0 {
// CHECK-DATA-STRUCTURES:   var predecessor: _AD__cond_bb3__Pred__src_0_wrt_0
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

// CHECK-SIL-LABEL: sil hidden [ossa] @AD__cond__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (@out Float, @owned @callee_guaranteed (@in_guaranteed Float) -> @out Float) {
// CHECK-SIL: bb0([[VJP_RESULT:%.*]] : $*Float, [[INPUT_ARG:%.*]] : $Float):
// CHECK-SIL:   [[BB0_PB_STRUCT:%.*]] = struct $_AD__cond_bb0__PB__src_0_wrt_0 ()
// CHECK-SIL:   cond_br {{%.*}}, bb1, bb3

// CHECK-SIL: bb1:
// CHECK-SIL:   [[BB1_PRED:%.*]] = enum $_AD__cond_bb1__Pred__src_0_wrt_0, #_AD__cond_bb1__Pred__src_0_wrt_0.bb0!enumelt.1, [[BB0_PB_STRUCT]]
// CHECK-SIL:   br bb2([[BB1_PRED]] : $_AD__cond_bb1__Pred__src_0_wrt_0)

// CHECK-SIL: bb2([[BB1_PRED_ARG:%.*]] : $_AD__cond_bb1__Pred__src_0_wrt_0)
// CHECK-SIL:   [[BB1_PB_STRUCT:%.*]] = struct $_AD__cond_bb1__PB__src_0_wrt_0
// CHECK-SIL:   [[BB3_PRED_PRED1:%.*]] = enum $_AD__cond_bb3__Pred__src_0_wrt_0, #_AD__cond_bb3__Pred__src_0_wrt_0.bb1!enumelt.1, [[BB1_PB_STRUCT]]
// CHECK-SIL:   br bb5({{.*}} : $Float, [[BB3_PRED_PRED1]] : $_AD__cond_bb3__Pred__src_0_wrt_0)

// CHECK-SIL: bb3:
// CHECK-SIL:   [[BB2_PRED:%.*]] = enum $_AD__cond_bb2__Pred__src_0_wrt_0, #_AD__cond_bb2__Pred__src_0_wrt_0.bb0!enumelt.1, [[BB0_PB_STRUCT]]
// CHECK-SIL:   br bb4([[BB2_PRED]] : $_AD__cond_bb2__Pred__src_0_wrt_0)

// CHECK-SIL: bb4([[BB2_PRED_ARG:%.*]] : $_AD__cond_bb2__Pred__src_0_wrt_0)
// CHECK-SIL:   [[BB2_PB_STRUCT:%.*]] = struct $_AD__cond_bb2__PB__src_0_wrt_0
// CHECK-SIL:   [[BB3_PRED_PRED2:%.*]] = enum $_AD__cond_bb3__Pred__src_0_wrt_0, #_AD__cond_bb3__Pred__src_0_wrt_0.bb2!enumelt.1, [[BB2_PB_STRUCT]]
// CHECK-SIL:   br bb5({{.*}} : $Float, [[BB3_PRED_PRED2]] : $_AD__cond_bb3__Pred__src_0_wrt_0)

// CHECK-SIL: bb5([[ORIG_RES:%.*]] : $Float, [[BB3_PRED_ARG:%.*]] : @owned $_AD__cond_bb3__Pred__src_0_wrt_0)
// CHECK-SIL:   [[BB3_PB_STRUCT:%.*]] = struct $_AD__cond_bb3__PB__src_0_wrt_0
// CHECK-SIL:   [[PULLBACK_REF:%.*]] = function_ref @AD__cond__pullback_src_0_wrt_0
// CHECK-SIL:   [[PB:%.*]] = partial_apply [callee_guaranteed] [[PULLBACK_REF]]([[BB3_PB_STRUCT]])
// CHECK-SIL:   store [[ORIG_RES]] to [trivial] [[VJP_RESULT]]
// CHECK-SIL:   return [[PB]]

// CHECK-SIL-LABEL: sil hidden [ossa] @AD__cond__pullback_src_0_wrt_0 : $@convention(thin) (@in_guaranteed Float, @owned _AD__cond_bb3__PB__src_0_wrt_0) -> @out Float {
// CHECK-SIL: bb0([[PB_RESULT:%.*]] : $*Float, [[SEED:%.*]] : $*Float, [[BB3_PB_STRUCT:%.*]] : @owned $_AD__cond_bb3__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr [[SEED]] to [initialization] [[SEED_COPY:%.*]] : $*Float
// CHECK-SIL:   [[BB3_PRED:%.*]] = destructure_struct [[BB3_PB_STRUCT]] : $_AD__cond_bb3__PB__src_0_wrt_0
// CHECK-SIL:   switch_enum [[BB3_PRED]] : $_AD__cond_bb3__Pred__src_0_wrt_0, case #_AD__cond_bb3__Pred__src_0_wrt_0.bb2!enumelt.1: bb3, case #_AD__cond_bb3__Pred__src_0_wrt_0.bb1!enumelt.1: bb1

// CHECK-SIL: bb1([[BB3_PRED1_TRAMP_PB_STRUCT:%.*]] : @owned $_AD__cond_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr [[SEED_COPY]] to [[ADJ_X_BB1:%.*]] : $*Float
// CHECK-SIL:   copy_addr {{%.*}} to [[ADJ_BUF2:%.*]] : $*Float
// CHECK-SIL:   br bb2([[BB3_PRED1_TRAMP_PB_STRUCT]] : $_AD__cond_bb1__PB__src_0_wrt_0)

// CHECK-SIL: bb2([[BB1_PB_STRUCT:%.*]] : @owned $_AD__cond_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   ([[BB1_PRED:%.*]], [[BB1_PB:%.*]]) = destructure_struct [[BB1_PB_STRUCT]]
// CHECK-SIL:   [[BUF0:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[BUF1:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[REABS_THUNK:%.*]] = function_ref @$sS3fIegydd_S3fIegnrr_TR
// CHECK-SIL:   [[BB1_PB_REABS:%.*]] = partial_apply [callee_guaranteed] [[REABS_THUNK]]([[BB1_PB]])
// CHECK-SIL:   [[BB1_ADJVALS:%.*]] = apply [[BB1_PB_REABS]]([[BUF0]], [[BUF1]], [[ADJ_X_BB1]]) : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @out Float)
// CHECK-SIL:   destroy_addr [[BUF0]] : $*Float
// CHECK-SIL:   destroy_addr [[BUF1]] : $*Float
// CHECK-SIL:   dealloc_stack [[BUF1]] : $*Float
// CHECK-SIL:   dealloc_stack [[BUF0]] : $*Float
// CHECK-SIL:   switch_enum [[BB1_PRED]] : $_AD__cond_bb1__Pred__src_0_wrt_0, case #_AD__cond_bb1__Pred__src_0_wrt_0.bb0!enumelt.1: bb5

// CHECK-SIL: bb3([[BB3_PRED2_TRAMP_PB_STRUCT:%.*]] : @owned $_AD__cond_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr [[SEED_COPY]] to [[ADJ_X_BB2:%.*]] : $*Float
// CHECK-SIL:   copy_addr {{%.*}} to [[ADJ_BUF3:%.*]] : $*Float
// CHECK-SIL:   br bb4([[BB3_PRED2_TRAMP_PB_STRUCT]] : $_AD__cond_bb2__PB__src_0_wrt_0)

// CHECK-SIL: bb4([[BB2_PB_STRUCT:%.*]] : @owned $_AD__cond_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   ([[BB2_PRED:%.*]], [[BB2_PB:%.*]]) = destructure_struct [[BB2_PB_STRUCT]]
// CHECK-SIL:   [[BUF0:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[BUF1:%.*]] = alloc_stack $Float
// CHECK-SIL:   [[REABS_THUNK:%.*]] = function_ref @$sS3fIegydd_S3fIegnrr_TR
// CHECK-SIL:   [[BB2_PB_REABS:%.*]] = partial_apply [callee_guaranteed] [[REABS_THUNK]]([[BB2_PB]])
// CHECK-SIL:   [[BB2_ADJVALS:%.*]] = apply [[BB2_PB_REABS]]([[BUF0]], [[BUF1]], [[ADJ_X_BB2]]) : $@callee_guaranteed (@in_guaranteed Float) -> (@out Float, @out Float)
// CHECK-SIL:   destroy_addr [[BUF0]] : $*Float
// CHECK-SIL:   destroy_addr [[BUF1]] : $*Float
// CHECK-SIL:   dealloc_stack [[BUF1]] : $*Float
// CHECK-SIL:   dealloc_stack [[BUF0]] : $*Float
// CHECK-SIL:   switch_enum [[BB2_PRED]] : $_AD__cond_bb2__Pred__src_0_wrt_0, case #_AD__cond_bb2__Pred__src_0_wrt_0.bb0!enumelt.1: bb6

// CHECK-SIL: bb5([[BB1_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   br bb7([[BB1_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb6([[BB2_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   br bb7([[BB2_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb7([[BB0_PB_STRUCT:%.*]] : $_AD__cond_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   destroy_addr {{%.*}} : $*Float
// CHECK-SIL:   dealloc_stack {{%.*}} : $*Float
// CHECK-SIL:   copy_addr [take] {{%.*}} to [initialization] [[PB_RESULT]]
// CHECK-SIL:   [[VOID:%.*]] = tuple ()
// CHECK-SIL:   return [[VOID]]

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
// Verify that pullback buffers are not allocated for address projections.

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

// CHECK-SIL-LABEL: sil hidden [ossa] @AD__cond_tuple_var__pullback_src_0_wrt_0 : $@convention(thin) (@in_guaranteed Float, @owned _AD__cond_tuple_var_bb3__PB__src_0_wrt_0) -> @out Float {
// CHECK-SIL: bb0([[PB_RESULT:%.*]] : $*Float, [[SEED:%.*]] : $*Float, [[BB3_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb3__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB3_PRED:%.*]] = destructure_struct [[BB3_PB_STRUCT]] : $_AD__cond_tuple_var_bb3__PB__src_0_wrt_0
// CHECK-SIL:   switch_enum [[BB3_PRED]] : $_AD__cond_tuple_var_bb3__Pred__src_0_wrt_0, case #_AD__cond_tuple_var_bb3__Pred__src_0_wrt_0.bb2!enumelt.1: bb3, case #_AD__cond_tuple_var_bb3__Pred__src_0_wrt_0.bb1!enumelt.1: bb1

// CHECK-SIL: bb1([[BB3_PRED1_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr {{%.*}} to {{%.*}} : $*(Float, Float)
// CHECK-SIL:   br bb2([[BB3_PRED1_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb1__PB__src_0_wrt_0)

// CHECK-SIL: bb2([[BB1_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb1__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB1_PRED:%.*]] = destructure_struct [[BB1_PB_STRUCT]]
// CHECK-SIL:   switch_enum [[BB1_PRED]] : $_AD__cond_tuple_var_bb1__Pred__src_0_wrt_0, case #_AD__cond_tuple_var_bb1__Pred__src_0_wrt_0.bb0!enumelt.1: bb5

// CHECK-SIL: bb3([[BB3_PRED2_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr {{%.*}} to {{%.*}} : $*(Float, Float)
// CHECK-SIL-NOT:   copy_addr {{%.*}} to {{%.*}} : $*Float
// CHECK-SIL:   br bb4([[BB3_PRED2_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb2__PB__src_0_wrt_0)

// CHECK-SIL: bb4([[BB2_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb2__PB__src_0_wrt_0):
// CHECK-SIL:   [[BB2_PRED:%.*]] = destructure_struct [[BB2_PB_STRUCT]]
// CHECK-SIL:   switch_enum [[BB2_PRED]] : $_AD__cond_tuple_var_bb2__Pred__src_0_wrt_0, case #_AD__cond_tuple_var_bb2__Pred__src_0_wrt_0.bb0!enumelt.1: bb6

// CHECK-SIL: bb5([[BB1_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr {{%.*}} to {{%.*}} : $*(Float, Float)
// CHECK-SIL-NOT:   copy_addr {{%.*}} to {{%.*}} : $*Float
// CHECK-SIL:   br bb7([[BB1_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb6([[BB2_PRED0_TRAMP_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr {{%.*}} to {{%.*}} : $*(Float, Float)
// CHECK-SIL-NOT:   copy_addr {{%.*}} to {{%.*}} : $*Float
// CHECK-SIL:   br bb7([[BB2_PRED0_TRAMP_PB_STRUCT]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0)

// CHECK-SIL: bb7([[BB0_PB_STRUCT:%.*]] : $_AD__cond_tuple_var_bb0__PB__src_0_wrt_0):
// CHECK-SIL:   copy_addr [take] {{%.*}} to [initialization] [[PB_RESULT]]
// CHECK-SIL:   [[VOID:%.*]] = tuple ()
// CHECK-SIL:   return [[VOID]]
