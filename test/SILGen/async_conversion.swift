// RUN: %target-swift-frontend -emit-silgen %s -module-name test -swift-version 5  -disable-availability-checking | %FileCheck %s
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

protocol P {
  func f(_: Int, _: String) async -> String?
}

struct X: P {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s4test1XVAA1PA2aDP1fySSSgSi_SStYaFTW : $@convention(witness_method: P) @async (Int, @guaranteed String, @in_guaranteed X) -> @owned Optional<String>
  // CHECK: function_ref @$s4test1XV1fySSSgSi_SStF : $@convention(method) (Int, @guaranteed String, X) -> @owned Optional<String>
  func f(_: Int, _: String) -> String? { nil }
}
