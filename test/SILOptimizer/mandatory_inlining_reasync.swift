// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -enable-experimental-concurrency -target %target-swift-5.1-abi-triple %s | %FileCheck %s
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
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]() : $@convention(thin) () -> Int
// CHECK-NEXT:  return [[RESULT]] : $Int
func callsReasyncFunction() -> Int {
  return reasyncFunction(nil, { return 321 } )
}
