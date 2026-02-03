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

// CHECK-LABEL: sil private [thunk] [ossa] @$s34vtable_thunks_reabstraction_modify12DerivedClassC8callbackyxSicvxAA04BaseF0CADyq_xcvxTV :
// CHECK: bb0
// CHECK-NEXT: // function_ref DerivedClass.callback.yielding_mutate
// CHECK-NEXT: [[DERIVED:%.*]] = function_ref @$s34vtable_thunks_reabstraction_modify12DerivedClassC8callbackyxSicvx :
// CHECK-NEXT: ([[RESULT_BUF:%.*]], [[TOKEN:%.*]], [[ALLOCATION:%.*]]) = begin_apply [[DERIVED]]<τ_0_0>(%0) :
// CHECK-NEXT: [[OUTER_RESULT_BUF:%.*]] = alloc_stack
// CHECK-NEXT: [[RESULT:%.*]] = load [take] [[RESULT_BUF]] :
// CHECK-NEXT: [[RESULT_CONV:%.*]] = convert_function [[RESULT]]
// CHECK-NEXT: // function_ref thunk
// CHECK-NEXT: [[THUNK_FN:%.*]] = function_ref @$sSixIegyr_SixIegnr_lTR :
// CHECK-NEXT: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<τ_0_0>([[RESULT_CONV]]) :
// CHECK-NEXT: [[THUNK_CONV:%.*]] = convert_function [[THUNK]]
// CHECK-NEXT: store [[THUNK_CONV]] to [init] [[OUTER_RESULT_BUF]] :
// CHECK-NEXT: yield [[OUTER_RESULT_BUF]] :

// CHECK: bb1:
// CHECK-NEXT: [[MODIFIED:%.*]] = load [take] [[OUTER_RESULT_BUF]] :
// CHECK-NEXT: [[MOD_CONV:%.*]] = convert_function [[MODIFIED]]
// CHECK-NEXT: // function_ref thunk
// CHECK-NEXT: [[THUNK_FN:%.*]] = function_ref @$sSixIegnr_SixIegyr_lTR :
// CHECK-NEXT: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<τ_0_0>([[MOD_CONV]]) :
// CHECK-NEXT: [[THUNK_CONV:%.*]] = convert_function [[THUNK]]
// CHECK-NEXT: store [[THUNK_CONV]] to [init] [[RESULT_BUF]] :
// CHECK-NEXT: dealloc_stack [[OUTER_RESULT_BUF]] :
// CHECK-NEXT: end_apply [[TOKEN]]
// CHECK-NEXT: dealloc_stack [[ALLOCATION]]
// CHECK-NEXT: [[RETURN_VAL:%.*]] = tuple ()
// CHECK-NEXT: return [[RETURN_VAL]]
