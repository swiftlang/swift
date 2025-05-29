// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir %s

public class BaseClass<Input, Result> {
  var callback: (Input) -> Result

  init(callback: @escaping (Input) -> Result) {
    self.callback = callback
  }
}

public class DerivedClass<Result> : BaseClass<Int, Result> {
  override var callback: (Int) -> Result {
    didSet {}
  }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s34vtable_thunks_reabstraction_modify12DerivedClassC8callbackyxSicvMAA04BaseF0CADyq_xcvMTV :
// CHECK: [[DERIVED:%.*]] = function_ref @$s34vtable_thunks_reabstraction_modify12DerivedClassC8callbackyxSicvM :
// CHECK: ([[RESULT_BUF:%.*]], [[TOKEN:%.*]]) = begin_apply [[DERIVED]]<τ_0_0>(%0) :
// CHECK: [[OUTER_RESULT_BUF:%.*]] = alloc_stack
// CHECK: [[RESULT:%.*]] = load [take] [[RESULT_BUF]] :
// CHECK: [[RESULT_CONV:%.*]] = convert_function [[RESULT]]
// CHECK: [[THUNK_FN:%.*]] = function_ref @$sSixIegyr_SixIegnr_lTR :
// CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<τ_0_0>([[RESULT_CONV]]) :
// CHECK: [[THUNK_CONV:%.*]] = convert_function [[THUNK]]
// CHECK: store [[THUNK_CONV]] to [init] [[OUTER_RESULT_BUF]] :
// CHECK: yield [[OUTER_RESULT_BUF]] :

// CHECK: bb1:
// CHECK: [[MODIFIED:%.*]] = load [take] [[OUTER_RESULT_BUF]] :
// CHECK: [[MOD_CONV:%.*]] = convert_function [[MODIFIED]]
// CHECK: [[THUNK_FN:%.*]] = function_ref @$sSixIegnr_SixIegyr_lTR :
// CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<τ_0_0>([[MOD_CONV]]) :
// CHECK: [[THUNK_CONV:%.*]] = convert_function [[THUNK]]
// CHECK: store [[THUNK_CONV]] to [init] [[RESULT_BUF]] :
// CHECK: dealloc_stack [[OUTER_RESULT_BUF]] :
// CHECK: end_apply [[TOKEN]]
// CHECK: return
