// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

struct NC: ~Copyable {
  borrowing func test() {}
  consuming func take() {}
}

func calledAtMostOnce(_ fn: @called(atMostOnce) () -> Void) { fn() }
func calledAtMostOnceThrowing(_ fn: @called(atMostOnce) () -> Void) throws { fn() }

// CHECK-LABEL: sil hidden @$s43called_at_most_once_closure_stack_promotion4test3nc1yAA2NCV_tKF : $@convention(thin) (@guaranteed NC) -> @error any Error {
// CHECK: bb0([[NC:%.*]] : $NC):
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s43called_at_most_once_closure_stack_promotion4test3nc1yAA2NCV_tKFyyXEfU_
// CHECK:  [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK:  [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK:  [[CALLEE:%.*]] = function_ref @$s43called_at_most_once_closure_stack_promotion0A18AtMostOnceThrowingyyyyXEnKF
// CHECK:  try_apply [[CALLEE]]([[DEP]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
//
// CHECK: [[NORMAL_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
//
// CHECK: [[ERROR_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: } // end sil function '$s43called_at_most_once_closure_stack_promotion4test3nc1yAA2NCV_tKF'
func test(nc1: borrowing NC) throws {
  try calledAtMostOnceThrowing {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s43called_at_most_once_closure_stack_promotion17testLocalVariableyyF : $@convention(thin) () -> () {
// CHECK: [[NC_STACK:%.*]] = alloc_stack [lexical] [var_decl] $NC, let, name "nc1"
// CHECK: [[CLOSURE:%.*]] = function_ref @$s43called_at_most_once_closure_stack_promotion17testLocalVariableyyFyyXEfU_
// CHECK: [[NC:%.*]] = load [[NC_STACK]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK: [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s43called_at_most_once_closure_stack_promotion0A10AtMostOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: dealloc_stack [[NC_STACK]]
// CHECK: } // end sil function '$s43called_at_most_once_closure_stack_promotion17testLocalVariableyyF'
func testLocalVariable() {
  let nc1 = NC()
  calledAtMostOnce {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s43called_at_most_once_closure_stack_promotion30testConsumingCaptureIsPromotedyyF : $@convention(thin) () -> () {
// CHECK: [[NC_STACK:%.*]] = alloc_stack [lexical] [var_decl] $NC, let, name "nc1"
// CHECK: [[CLOSURE:%.*]] = function_ref @$s43called_at_most_once_closure_stack_promotion30testConsumingCaptureIsPromotedyyFyyXEfU_
// CHECK: [[NC:%.*]] = load [[NC_STACK]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@owned NC) -> ()
// CHECK: [[CALLEE:%.*]] = function_ref @$s43called_at_most_once_closure_stack_promotion0A10AtMostOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[PA]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: dealloc_stack [[NC_STACK]]
// CHECK: } // end sil function '$s43called_at_most_once_closure_stack_promotion30testConsumingCaptureIsPromotedyyF'
func testConsumingCaptureIsPromoted() {
  let nc1 = NC()
  calledAtMostOnce {
    nc1.take()
  }
}
