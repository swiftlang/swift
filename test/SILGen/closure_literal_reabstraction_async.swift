// RUN: %target-swift-frontend -emit-silgen %s -disable-availability-checking | %FileCheck %s
// REQUIRES: concurrency

@_silgen_name("takeThrowingAsyncClosure")
func takeThrowingAsyncClosure<T>(_: () async throws -> T) 

// CHECK-LABEL: sil {{.*}} @{{.*}}34passNonthrowingAsyncClosureLiteral
func passNonthrowingAsyncClosureLiteral() {
    // Check that the literal closure was emitted directly with an error return,
    // without a reabstraction thunk to convert from nonthrowing.
    // CHECK: [[INVOKE_FN:%.*]] = function_ref
    // CHECK: [[CLOSURE:%.*]] = thin_to_thick_function [[INVOKE_FN]]
    // CHECK: [[CALLEE:%.*]] = function_ref @takeThrowingAsyncClosure
    // CHECK: apply [[CALLEE]]<Int>([[CLOSURE]])
    takeThrowingAsyncClosure { return 42 }
}
