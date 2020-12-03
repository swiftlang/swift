// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5 -enable-experimental-concurrency | %FileCheck %s
// REQUIRES: concurrency

func f(_: Int, _: String) -> String? { nil }

// CHECK-LABEL: sil hidden [ossa] @$s4testAAyyF : $@convention(thin) () -> () {
func test() {
  // CHECK: [[F:%.*]] = function_ref @$s4test1fySSSgSi_SStF : $@convention(thin) (Int, @guaranteed String) -> @owned Optional<String>
  // CHECK: [[THICK_F:%.*]] = thin_to_thick_function [[F]] : $@convention(thin) (Int, @guaranteed String) -> @owned Optional<String> to $@callee_guaranteed (Int, @guaranteed String) -> @owned Optional<String>
  // CHECK: [[THUNK:%.*]] = function_ref @$sSiS2SSgIegygo_SiSSAAIegHygo_TR : $@convention(thin) @async (Int, @guaranteed String, @guaranteed @callee_guaranteed (Int, @guaranteed String) -> @owned Optional<String>) -> @owned Optional<String>
  // CHECK: partial_apply [callee_guaranteed] [[THUNK]]([[THICK_F]]) : $@convention(thin) @async (Int, @guaranteed String, @guaranteed @callee_guaranteed (Int, @guaranteed String) -> @owned Optional<String>) -> @owned Optional<String>
  let _: (Int, String) async -> String? = f
}
