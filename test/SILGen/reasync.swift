// RUN: %target-swift-frontend -emit-silgen %s -enable-experimental-concurrency -disable-availability-checking | %FileCheck %s
// REQUIRES: concurrency

// CHECK-LABEL: sil hidden [ossa] @$s7reasync0A8FunctionyyyyYaXEYaF : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> ()) -> () {
func reasyncFunction(_ a: () async -> ()) reasync {
  await a()
}

// CHECK-LABEL: sil hidden [ossa] @$s7reasync10syncCalleryyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> () {
// CHECK: [[THUNK_FN:%.*]] = function_ref @$sIg_IegH_TR : $@convention(thin) @async (@noescape @callee_guaranteed () -> ()) -> ()
// CHECK: [[THUNK:%.*]] = partial_apply [callee_guaranteed] [[THUNK_FN]](%0) : $@convention(thin) @async (@noescape @callee_guaranteed () -> ()) -> ()
// CHECK: [[NOESCAPE:%.*]] = convert_escape_to_noescape [not_guaranteed] [[THUNK]] : $@async @callee_guaranteed () -> () to $@noescape @async @callee_guaranteed () -> ()
// CHECK: [[FN:%.*]] = function_ref @$s7reasync0A8FunctionyyyyYaXEYaF : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK: apply [noasync] [[FN]]([[NOESCAPE]]) : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> ()) -> ()
func syncCaller(_ fn: () -> ()) {
  reasyncFunction(fn)
}

func asyncCaller(_ fn: () async -> ()) async {
// CHECK: [[FN:%.*]] = function_ref @$s7reasync0A8FunctionyyyyYaXEYaF : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> ()) -> ()
// CHECK: apply [[FN]](%0) : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> ()) -> ()
   await reasyncFunction(fn)
}

// CHECK-LABEL: sil hidden [ossa] @$s7reasync23throwingReasyncFunctionyyyyYaKXEYaKF : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> @error Error) -> @error Error {
func throwingReasyncFunction(_ a: () async throws -> ()) reasync throws {
	try await a()
}

// CHECK-LABEL: sil hidden [ossa] @$s7reasync18throwingSyncCalleryyyyXEKF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> @error Error {
// CHECK: [[FN:%.*]] = function_ref @$s7reasync23throwingReasyncFunctionyyyyYaKXEYaKF : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> @error Error) -> @error Error
// CHECK: try_apply [noasync] [[FN]]({{.*}}) : $@convention(thin) @async (@noescape @async @callee_guaranteed () -> @error Error) -> @error Error, normal bb1, error bb2
func throwingSyncCaller(_ fn: () -> ()) throws {
	try throwingReasyncFunction(fn)
}
