// RUN: %target-swift-emit-silgen %s | %FileCheck %s
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

// CHECK-LABEL: sil private [thunk] [ossa] @$s34vtable_thunks_reabstraction_modify12DerivedClassC8callbackyxSicvMAA04BaseF0CADyq_xcvMTV : $@yield_once @convention(method) <Result> (@guaranteed DerivedClass<Result>) -> @yields @inout @callee_guaranteed (@in_guaranteed Int) -> @out Result {
// CHECK: [[DERIVED:%.*]] = function_ref @$s34vtable_thunks_reabstraction_modify12DerivedClassC8callbackyxSicvM : $@yield_once @convention(method) <τ_0_0> (@guaranteed DerivedClass<τ_0_0>) -> @yields @inout @callee_guaranteed (Int) -> @out τ_0_0
// CHECK: ([[RESULT_BUF:%.*]], [[TOKEN:%.*]]) = begin_apply [[DERIVED]]<Result>(%0) : $@yield_once @convention(method) <τ_0_0> (@guaranteed DerivedClass<τ_0_0>) -> @yields @inout @callee_guaranteed (Int) -> @out τ_0_0
// CHECK: [[OUTER_RESULT_BUF:%.*]] = alloc_stack $@callee_guaranteed (@in_guaranteed Int) -> @out Result
// CHECK: [[RESULT:%.*]] = load [take] [[RESULT_BUF]] : $*@callee_guaranteed (Int) -> @out Result
// CHECK: [[THUNK_FN:%.*]] = function_ref @$sSixIegyr_SixIegnr_lTR : $@convention(thin) <τ_0_0> (@in_guaranteed Int, @guaranteed @callee_guaranteed (Int) -> @out τ_0_0) -> @out τ_0_0
// CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<Result>([[RESULT]]) : $@convention(thin) <τ_0_0> (@in_guaranteed Int, @guaranteed @callee_guaranteed (Int) -> @out τ_0_0) -> @out τ_0_0
// CHECK: store [[THUNK]] to [init] [[OUTER_RESULT_BUF]] : $*@callee_guaranteed (@in_guaranteed Int) -> @out Result
// CHECK: yield [[OUTER_RESULT_BUF]] : $*@callee_guaranteed (@in_guaranteed Int) -> @out Result, resume bb1, unwind bb2

// CHECK: bb1:
// CHECK: [[MODIFIED:%.*]] = load [take] [[OUTER_RESULT_BUF]] : $*@callee_guaranteed (@in_guaranteed Int) -> @out Result
// CHECK: [[THUNK_FN:%.*]] = function_ref @$sSixIegnr_SixIegyr_lTR : $@convention(thin) <τ_0_0> (Int, @guaranteed @callee_guaranteed (@in_guaranteed Int) -> @out τ_0_0) -> @out τ_0_0
// CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]<Result>([[MODIFIED]]) : $@convention(thin) <τ_0_0> (Int, @guaranteed @callee_guaranteed (@in_guaranteed Int) -> @out τ_0_0) -> @out τ_0_0
// CHECK: store [[THUNK]] to [init] [[RESULT_BUF]] : $*@callee_guaranteed (Int) -> @out Result
// CHECK: dealloc_stack [[OUTER_RESULT_BUF]] : $*@callee_guaranteed (@in_guaranteed Int) -> @out Result
// CHECK: end_apply [[TOKEN]]
// CHECK: return
