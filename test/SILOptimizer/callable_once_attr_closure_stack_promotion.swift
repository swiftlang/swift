// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature OnceFunctions %s | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions

struct NC: ~Copyable {
  borrowing func test() {}
  consuming func take() {}
}

func calledAtMostOnce(_ fn: @callable(once) () -> Void) { fn() }
func calledAtMostOnceThrowing(_ fn: @callable(once) () -> Void) throws { fn() }

// CHECK-LABEL: sil hidden @$s42callable_once_attr_closure_stack_promotion4test3nc1yAA2NCV_tKF : $@convention(thin) (@guaranteed NC) -> @error any Error {
// CHECK: bb0([[NC:%.*]] : $NC):
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion4test3nc1yAA2NCV_tKFyyXEfU_
// CHECK:  [[PA:%.*]] = partial_apply [on_stack] [called_at_most_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK:  [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK:  [[CALLEE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion24calledAtMostOnceThrowingyyyyXEnKF
// CHECK:  try_apply [[CALLEE]]([[DEP]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
//
// CHECK: [[NORMAL_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
//
// CHECK: [[ERROR_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: } // end sil function '$s42callable_once_attr_closure_stack_promotion4test3nc1yAA2NCV_tKF'
func test(nc1: borrowing NC) throws {
  try calledAtMostOnceThrowing {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s42callable_once_attr_closure_stack_promotion17testLocalVariableyyF : $@convention(thin) () -> () {
// CHECK: [[NC_STACK:%.*]] = alloc_stack [lexical] [var_decl] $NC, let, name "nc1"
// CHECK: [[CLOSURE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion17testLocalVariableyyFyyXEfU_
// CHECK: [[NC:%.*]] = load [[NC_STACK]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_at_most_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK: [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion16calledAtMostOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: dealloc_stack [[NC_STACK]]
// CHECK: } // end sil function '$s42callable_once_attr_closure_stack_promotion17testLocalVariableyyF'
func testLocalVariable() {
  let nc1 = NC()
  calledAtMostOnce {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s42callable_once_attr_closure_stack_promotion30testConsumingCaptureIsPromotedyyF : $@convention(thin) () -> () {
// CHECK: [[NC_STACK:%.*]] = alloc_stack [lexical] [var_decl] $NC, let, name "nc1"
// CHECK: [[CLOSURE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion30testConsumingCaptureIsPromotedyyFyyXEfU_
// CHECK: [[NC:%.*]] = load [[NC_STACK]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_at_most_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@owned NC) -> ()
// CHECK: [[CALLEE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion16calledAtMostOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[PA]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: dealloc_stack [[NC_STACK]]
// CHECK: } // end sil function '$s42callable_once_attr_closure_stack_promotion30testConsumingCaptureIsPromotedyyF'
func testConsumingCaptureIsPromoted() {
  let nc1 = NC()
  calledAtMostOnce {
    nc1.take()
  }
}

struct GenericNC<T>: ~Copyable {
  var t: T
  consuming func take() {}
  mutating func mutate() {}
}

// A noncopyable address-only value that the closure consumes is moved into the
// closure's on-stack context, which owns it, so nothing destroys the
// temporary after the call.
//
// CHECK-LABEL: sil hidden @$s42callable_once_attr_closure_stack_promotion27testGenericConsumingCaptureyyAA0H2NCVyxGnlF : $@convention(thin) <T> (@in GenericNC<T>) -> () {
// CHECK: [[TEMP:%.*]] = alloc_stack $GenericNC<T>
// CHECK: copy_addr [take] {{%.*}} to [init] [[TEMP]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_at_most_once] {{%.*}}<T>([[TEMP]]) : $@convention(thin) <τ_0_0> (@in GenericNC<τ_0_0>) -> ()
// CHECK: [[DEP:%.*]] = mark_dependence [nonescaping] [[PA]] on [[TEMP]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion16calledAtMostOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK: } // end sil function '$s42callable_once_attr_closure_stack_promotion27testGenericConsumingCaptureyyAA0H2NCVyxGnlF'
func testGenericConsumingCapture<T>(_ nc: consuming GenericNC<T>) {
  calledAtMostOnce {
    nc.take()
  }
}

// The closure's on-stack context owns the consumed value, but only holds the
// address of the value that it captures inout. Nothing destroys either value
// after the call.
//
// CHECK-LABEL: sil hidden @$s42callable_once_attr_closure_stack_promotion36testGenericConsumingAndInoutCapturesyyAA0H2NCVyxGn_AEztlF : $@convention(thin) <T> (@in GenericNC<T>, @inout GenericNC<T>) -> () {
// CHECK: bb0({{%.*}} : $*GenericNC<T>, [[OTHER:%.*]] : $*GenericNC<T>):
// CHECK: [[TEMP:%.*]] = alloc_stack $GenericNC<T>
// CHECK: copy_addr [take] {{%.*}} to [init] [[TEMP]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_at_most_once] {{%.*}}<T>([[TEMP]], [[OTHER]]) : $@convention(thin) <τ_0_0> (@in GenericNC<τ_0_0>, @inout_aliasable GenericNC<τ_0_0>) -> ()
// CHECK: [[DEP1:%.*]] = mark_dependence [nonescaping] [[PA]] on [[TEMP]]
// CHECK: [[DEP2:%.*]] = mark_dependence [nonescaping] [[DEP1]] on [[OTHER]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s42callable_once_attr_closure_stack_promotion16calledAtMostOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP2]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK-NOT: destroy_addr
// CHECK: } // end sil function '$s42callable_once_attr_closure_stack_promotion36testGenericConsumingAndInoutCapturesyyAA0H2NCVyxGn_AEztlF'
func testGenericConsumingAndInoutCaptures<T>(_ nc: consuming GenericNC<T>, _ other: inout GenericNC<T>) {
  calledAtMostOnce {
    nc.take()
    other.mutate()
  }
}
