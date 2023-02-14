// RUN: %target-swift-frontend -emit-sil -enable-experimental-concurrency -disable-availability-checking %s | %FileCheck %s
// REQUIRES: concurrency

// REQUIRES: swift_in_compiler

@_transparent
func reasyncFunction(_ value: Optional<Int>, _ fn: () async throws -> Int) reasync rethrows -> Int {
  switch value {
  case .some(let x): return x
  case .none: return try await fn()
  }
}

// CHECK-LABEL: sil hidden @$s26mandatory_inlining_reasync20callsReasyncFunctionSiyF : $@convention(thin) () -> Int {
// CHECK: [[FN:%.*]] = function_ref @$s26mandatory_inlining_reasync20callsReasyncFunctionSiyFSiyXEfU_ : $@convention(thin) () -> Int
// CHECK-NEXT: [[THICK:%.*]] = thin_to_thick_function [[FN]] : $@convention(thin) () -> Int to $@noescape @callee_guaranteed () -> Int
// CHECK-NEXT: [[RESULT:%.*]] = apply [[THICK]]() : $@noescape @callee_guaranteed () -> Int
// CHECK-NEXT:  return [[RESULT]] : $Int
func callsReasyncFunction() -> Int {
  return reasyncFunction(nil, { return 321 } )
}
REQUIRES: updating_for_owned_noescape
