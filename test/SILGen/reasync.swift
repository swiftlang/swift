// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -enable-experimental-concurrency -target %target-swift-5.1-abi-triple | %FileCheck %s
// REQUIRES: concurrency

// CHECK-LABEL: sil hidden [ossa] @$s7reasync0A8FunctionyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> () {
func reasyncFunction(_ a: () async -> ()) reasync {
  await a()
}

// CHECK-LABEL: sil hidden [ossa] @$s7reasync10syncCalleryyyyXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> ()) -> () {
// CHECK: [[ARGC:%.*]] = copy_value %0
// CHECK: [[THUNK_FN:%.*]] = function_ref @$sIg_IegH_TR : $@convention(thin) @async (@guaranteed @noescape @callee_guaranteed () -> ()) -> ()
// CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]]([[ARGC]]) : $@convention(thin) @async (@guaranteed @noescape @callee_guaranteed () -> ()) -> ()
// CHECK: [[NOESCAPE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[THUNK]] : $@async @callee_guaranteed () -> () to $@noescape @async @callee_guaranteed () -> ()
// CHECK: [[FN:%.*]] = function_ref @$s7reasync0A8FunctionyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK: apply [noasync] [[FN]]([[NOESCAPE]]) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
func syncCaller(_ fn: () -> ()) {
  reasyncFunction(fn)
}

func asyncCaller(_ fn: () async -> ()) async {
// CHECK: [[FN:%.*]] = function_ref @$s7reasync0A8FunctionyyyyYaXEYaF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK: apply [[FN]](%0) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> ()) -> ()
   await reasyncFunction(fn)
}

// CHECK-LABEL: sil hidden [ossa] @$s7reasync23throwingReasyncFunctionyyyyYaKXEYaKF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> @error any Error) -> @error any Error {
func throwingReasyncFunction(_ a: () async throws -> ()) reasync throws {
	try await a()
}

// CHECK-LABEL: sil hidden [ossa] @$s7reasync18throwingSyncCalleryyyyXEKF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed () -> ()) -> @error any Error {
// CHECK: [[FN:%.*]] = function_ref @$s7reasync23throwingReasyncFunctionyyyyYaKXEYaKF : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> @error any Error) -> @error any Error
// CHECK: try_apply [noasync] [[FN]]({{.*}}) : $@convention(thin) @async (@guaranteed @noescape @async @callee_guaranteed () -> @error any Error) -> @error any Error, normal bb1, error bb2
func throwingSyncCaller(_ fn: () -> ()) throws {
	try throwingReasyncFunction(fn)
}
