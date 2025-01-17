// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -Xllvm -sil-print-types -emit-silgen -verify %s | %FileCheck %s
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -Xllvm -sil-print-types -emit-silgen -verify -strict-concurrency=complete %s | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: concurrency

@MainActor
protocol P {
  func test() async
}

struct S : P {
  func test() {}
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s10issue623941SVAA1PA2aDP4testyyYaFTW : $@convention(witness_method: P) @async (@in_guaranteed S) -> ()
// CHECK: [[MAIN_ACTOR:%.*]] = begin_borrow {{.*}} : $MainActor
// CHECK-NEXT: hop_to_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[SELF:%.*]] = load [trivial] %0 : $*S
// CHECK: [[TEST_FN:%.*]] = function_ref @$s10issue623941SV4testyyF : $@convention(method) (S) -> ()
// CHECK-NEXT: {{.*}} = apply [[TEST_FN]]([[SELF]]) : $@convention(method) (S) -> ()
