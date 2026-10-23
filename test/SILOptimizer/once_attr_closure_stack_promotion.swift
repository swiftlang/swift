// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature OnceFunctions %s | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions

struct NC: ~Copyable {
  borrowing func test() {}
  consuming func take() {}
}

func calledExactlyOnce(_ fn: @once () -> Void) { fn() }
func calledExactlyOnceThrowing(_ fn: @once () -> Void) throws { fn() }

// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion4test3nc1yAA2NCV_tKF : $@convention(thin) (@guaranteed NC) -> @error any Error {
// CHECK: bb0([[NC:%.*]] : $NC):
// CHECK:  [[CLOSURE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion4test3nc1yAA2NCV_tKFyyXEfU_
// CHECK:  [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK:  [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK:  [[CALLEE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion25calledExactlyOnceThrowingyyyyXEnKF
// CHECK:  try_apply [[CALLEE]]([[DEP]]) : {{.*}}, normal [[NORMAL_BB:bb[0-9]+]], error [[ERROR_BB:bb[0-9]+]]
//
// CHECK: [[NORMAL_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
//
// CHECK: [[ERROR_BB]]({{.*}}):
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion4test3nc1yAA2NCV_tKF'
func test(nc1: borrowing NC) throws {
  try calledExactlyOnceThrowing {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion17testLocalVariableyyF : $@convention(thin) () -> () {
// CHECK: [[NC_STACK:%.*]] = alloc_stack [lexical] [var_decl] $NC, let, name "nc1"
// CHECK: [[CLOSURE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion17testLocalVariableyyFyyXEfU_
// CHECK: [[NC:%.*]] = load [[NC_STACK]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@guaranteed NC) -> ()
// CHECK: [[DEP:%.*]] = mark_dependence [[PA]] on [[NC]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion17calledExactlyOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: dealloc_stack [[NC_STACK]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion17testLocalVariableyyF'
func testLocalVariable() {
  let nc1 = NC()
  calledExactlyOnce {
    nc1.test()
  }
}

// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion30testConsumingCaptureIsPromotedyyF : $@convention(thin) () -> () {
// CHECK: [[NC_STACK:%.*]] = alloc_stack [lexical] [var_decl] $NC, let, name "nc1"
// CHECK: [[CLOSURE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion30testConsumingCaptureIsPromotedyyFyyXEfU_
// CHECK: [[NC:%.*]] = load [[NC_STACK]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_once] [[CLOSURE]]([[NC]]) : $@convention(thin) (@owned NC) -> ()
// CHECK: [[CALLEE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion17calledExactlyOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[PA]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK: dealloc_stack [[NC_STACK]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion30testConsumingCaptureIsPromotedyyF'
func testConsumingCaptureIsPromoted() {
  let nc1 = NC()
  calledExactlyOnce {
    nc1.take()
  }
}

struct GenericNC<T>: ~Copyable {
  var t: T
  consuming func take() {}
  mutating func mutate() {}
}

// An address-only noncopyable value that the closure consumes is moved into the
// closure's on-stack context, which owns it, so nothing destroys the
// temporary after the call.
//
// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion27testGenericConsumingCaptureyyAA0G2NCVyxGnlF : $@convention(thin) <T> (@in GenericNC<T>) -> () {
// CHECK: [[TEMP:%.*]] = alloc_stack $GenericNC<T>
// CHECK: copy_addr [take] {{%.*}} to [init] [[TEMP]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_once] {{%.*}}<T>([[TEMP]]) : $@convention(thin) <τ_0_0> (@in GenericNC<τ_0_0>) -> ()
// CHECK: [[DEP:%.*]] = mark_dependence [nonescaping] [[PA]] on [[TEMP]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion17calledExactlyOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion27testGenericConsumingCaptureyyAA0G2NCVyxGnlF'
func testGenericConsumingCapture<T>(_ nc: consuming GenericNC<T>) {
  calledExactlyOnce {
    nc.take()
  }
}

// The closure's on-stack context owns the consumed value, but only holds the
// address of the value that it captures inout. Nothing destroys either value
// after the call.
//
// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion36testGenericConsumingAndInoutCapturesyyAA0G2NCVyxGn_AEztlF : $@convention(thin) <T> (@in GenericNC<T>, @inout GenericNC<T>) -> () {
// CHECK: bb0({{%.*}} : $*GenericNC<T>, [[OTHER:%.*]] : $*GenericNC<T>):
// CHECK: [[TEMP:%.*]] = alloc_stack $GenericNC<T>
// CHECK: copy_addr [take] {{%.*}} to [init] [[TEMP]]
// CHECK: [[PA:%.*]] = partial_apply [on_stack] [called_once] {{%.*}}<T>([[TEMP]], [[OTHER]]) : $@convention(thin) <τ_0_0> (@in GenericNC<τ_0_0>, @inout_aliasable GenericNC<τ_0_0>) -> ()
// CHECK: [[DEP1:%.*]] = mark_dependence [nonescaping] [[PA]] on [[TEMP]]
// CHECK: [[DEP2:%.*]] = mark_dependence [nonescaping] [[DEP1]] on [[OTHER]]
// CHECK: [[CALLEE:%.*]] = function_ref @$s33once_attr_closure_stack_promotion17calledExactlyOnceyyyyXEnF
// CHECK: apply [[CALLEE]]([[DEP2]])
// CHECK-NEXT: dealloc_stack [[PA]]
// CHECK-NEXT: dealloc_stack [[TEMP]]
// CHECK-NOT: destroy_addr
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion36testGenericConsumingAndInoutCapturesyyAA0G2NCVyxGn_AEztlF'
func testGenericConsumingAndInoutCaptures<T>(_ nc: consuming GenericNC<T>, _ other: inout GenericNC<T>) {
  calledExactlyOnce {
    nc.take()
    other.mutate()
  }
}

func genericCalledExactlyOnce<R>(_ fn: @once () -> R) -> R { fn() }

func makeInt() -> Int { 42 }

// The function value is reabstracted by an exactly-once thunk that captures
// another one.
// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion29testFunctionValueThroughThunkSiyF : $@convention(thin) () -> Int {
// CHECK:   [[INNER:%.*]] = partial_apply [on_stack] [called_once] {{%.*}}({{%.*}}) : $@convention(thin) (@guaranteed @callee_guaranteed () -> Int) -> Int
// CHECK:   [[OUTER:%.*]] = partial_apply [on_stack] [called_once] {{%.*}}([[INNER]]) : $@convention(thin) (@owned @noescape @once @callee_owned () -> Int) -> @out Int
// CHECK:   apply
// CHECK:   dealloc_stack [[OUTER]]
// CHECK:   dealloc_stack [[INNER]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion29testFunctionValueThroughThunkSiyF'
func testFunctionValueThroughThunk() -> Int {
  genericCalledExactlyOnce(makeInt)
}

@_transparent
func transparentCalledExactlyOnce<R>(_ fn: @once () -> R) -> R { fn() }

// Once inlined, the closure is stored into the stack slot of the consuming
// parameter.
// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion29testInlinedConsumingParameteryS2iF : $@convention(thin) (Int) -> Int {
// CHECK:   [[PA:%.*]] = partial_apply [on_stack] [called_once]
// CHECK:   [[SLOT:%.*]] = alloc_stack [lexical] [var_decl] $@noescape @once @callee_owned
// CHECK:   store [[PA]] to [[SLOT]]
// CHECK:   apply
// CHECK:   dealloc_stack [[SLOT]]
// CHECK:   dealloc_stack [[PA]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion29testInlinedConsumingParameteryS2iF'
func testInlinedConsumingParameter(_ x: Int) -> Int {
  transparentCalledExactlyOnce { x + 1 }
}

@_transparent
func transparentRebound(_ fn: @once () -> Int) -> Int {
  let g = fn
  return g()
}

// Once inlined, the closure is moved from the stack slot of the parameter into
// the stack slot of `g`.
// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion20testInlinedRebindingyS2iF : $@convention(thin) (Int) -> Int {
// CHECK:   [[PA:%.*]] = partial_apply [on_stack] [called_once]
// CHECK:   [[SLOT:%.*]] = alloc_stack [lexical] [var_decl] $@noescape @once @callee_owned
// CHECK:   store [[PA]] to [[SLOT]]
// CHECK:   [[G:%.*]] = alloc_stack [lexical] [var_decl] $@noescape @once @callee_owned
// CHECK:   copy_addr [take] {{%.*}} to [init] [[G]]
// CHECK:   apply
// CHECK:   dealloc_stack [[G]]
// CHECK:   dealloc_stack [[SLOT]]
// CHECK:   dealloc_stack [[PA]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion20testInlinedRebindingyS2iF'
func testInlinedRebinding(_ x: Int) -> Int {
  transparentRebound { x + 1 }
}

@_transparent
func transparentForwarded(_ fn: @once () -> Int) -> Int {
  transparentRebound(fn)
}

// Once inlined, the closure is taken out of the stack slot of one parameter
// and stored into the stack slot of another.
// CHECK-LABEL: sil hidden @$s33once_attr_closure_stack_promotion21testInlinedForwardingyS2iF : $@convention(thin) (Int) -> Int {
// CHECK:   [[PA:%.*]] = partial_apply [on_stack] [called_once]
// CHECK:   apply
// CHECK:   dealloc_stack [[PA]]
// CHECK: } // end sil function '$s33once_attr_closure_stack_promotion21testInlinedForwardingyS2iF'
func testInlinedForwarding(_ x: Int) -> Int {
  transparentForwarded { x + 1 }
}
