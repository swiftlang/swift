// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute

struct NC: ~Copyable {
  borrowing func test() {}
  consuming func take() {}
}

func calledOnce(_ fn: @called(once) () -> Void) { fn() }
func calledOnceThrowing(_ fn: @called(once) () -> Void) throws { fn() }

// CHECK-LABEL: sil hidden @$s35called_once_closure_stack_promotion4test3nc1yAA2NCV_tKF : $@convention(thin) (@guaranteed NC) -> @error any Error {
// CHECK: bb0([[NC:%.*]] : $NC):
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s35called_once_closure_stack_promotion4test3nc1yAA2NCV_tKFyyXEfU_
// CHECK:  [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK:  [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK:  [[CALLEE:%.*]] = function_ref @$s35called_once_closure_stack_promotion0A12OnceThrowingyyyyXEnKF
// CHECK:  try_apply [[CALLEE]]([[DEP]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
//
// CHECK: [[NORMAL_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
//
// CHECK: [[ERROR_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: } // end sil function '$s35called_once_closure_stack_promotion4test3nc1yAA2NCV_tKF'
func test(nc1: borrowing NC) throws {
  try calledOnceThrowing {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s35called_once_closure_stack_promotion17testLocalVariableyyF : $@convention(thin) () -> () {
// CHECK: [[NC_STACK:%.*]] = alloc_stack [lexical] [var_decl] $NC, let, name "nc1"
// CHECK: [[CLOSURE:%.*]] = function_ref @$s35called_once_closure_stack_promotion17testLocalVariableyyFyyXEfU_
// CHECK: [[NC:%.*]] = load [[NC_STACK]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK: [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s35called_once_closure_stack_promotion0A4OnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: dealloc_stack [[NC_STACK]]
// CHECK: } // end sil function '$s35called_once_closure_stack_promotion17testLocalVariableyyF'
func testLocalVariable() {
  let nc1 = NC()
  calledOnce {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s35called_once_closure_stack_promotion33testConsumingCaptureIsNotPromotedyyF : $@convention(thin) () -> () {
// CHECK: [[CLOSURE:%.*]] = function_ref @$s35called_once_closure_stack_promotion33testConsumingCaptureIsNotPromotedyyFyyXEfU_
// CHECK: [[NC:%.*]] = load {{%.*}}
// CHECK: [[PA:%.*]] = partial_apply [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@owned NC) -> ()
// CHECK-NOT: [on_stack]
// CHECK: convert_escape_to_noescape [[PA]]
// CHECK: } // end sil function '$s35called_once_closure_stack_promotion33testConsumingCaptureIsNotPromotedyyF'
func testConsumingCaptureIsNotPromoted() {
  let nc1 = NC()
  calledOnce {
    nc1.take()
  }
}
