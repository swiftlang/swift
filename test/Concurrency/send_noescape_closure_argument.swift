// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls -D NO_ERRORS %s -module-name main -o %t/send_noescape_closure_argument.sil
// RUN: %FileCheck %s < %t/send_noescape_closure_argument.sil
// RUN: %target-swift-frontend -emit-sil -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls %s -module-name main -verify -o /dev/null

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// A non-escaping closure argument of a 'send' call cannot be evaluated at the
// call site, because the escaping closure that runs the call later cannot
// capture it. It is formed in that closure instead, for a synchronous callee
// that is enqueued just like for a callee that runs in a task

actor Worker {
  func run(_ body: () -> Void) { body() }
  func run(_ value: Int, _ body: (Int) -> Void) { body(value) }
  func runAsync(_ body: () -> Void) async { body() }
}

@MainActor
func runOnMain(_ body: () -> Void) { body() }


#if NO_ERRORS

// CHECK-LABEL: sil hidden @$s4main18sendClosureToActoryyAA6WorkerCF :
// CHECK-NOT: builtin "createAsyncTask"
// CHECK: function_ref @$ss14_enqueueOneway2on_yx_yxYictScARzlF
// CHECK: } // end sil function '$s4main18sendClosureToActoryyAA6WorkerCF'
func sendClosureToActor(_ w: Worker) {
  send w.run { print("ran") }
}

// The value argument is still evaluated at the call site
// CHECK-LABEL: sil hidden @$s4main26sendValueAndClosureToActoryyAA6WorkerC_SitF :
// CHECK: function_ref @$ss14_enqueueOneway2on_yx_yxYictScARzlF
// CHECK: } // end sil function '$s4main26sendValueAndClosureToActoryyAA6WorkerC_SitF'
func sendValueAndClosureToActor(_ w: Worker, _ x: Int) {
  send w.run(x) { print($0) }
}

// CHECK-LABEL: sil hidden @$s4main22sendClosureToMainActoryyF :
// CHECK-NOT: builtin "createAsyncTask"
// CHECK: } // end sil function '$s4main22sendClosureToMainActoryyF'
func sendClosureToMainActor() {
  send runOnMain { print("ran") }
}

// An async callee runs in a task
// CHECK-LABEL: sil hidden @$s4main20sendClosureToAsyncFnyyAA6WorkerCF :
// CHECK: builtin "createAsyncTask
// CHECK: } // end sil function '$s4main20sendClosureToAsyncFnyyAA6WorkerCF'
func sendClosureToAsyncFn(_ w: Worker) {
  send w.runAsync { print("ran") }
}

#else

// A non-escaping parameter cannot be forwarded to a call that runs later
func forwardNonescapingParameter(_ w: Worker, _ body: () -> Void) {
  // expected-note@-1{{parameter 'body' is implicitly non-escaping}}
  send w.run(body) // expected-error{{escaping closure captures non-escaping parameter 'body'}} expected-note{{captured here}}
}

#endif
