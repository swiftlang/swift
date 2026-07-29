// RUN: %target-swift-emit-silgen %s -verify | %FileCheck %s

// REQUIRES: concurrency

protocol P {
  func test(isolation: isolated (any Actor)?) async
}

struct S: P {
  func test(isolation: isolated (any Actor)?) {
  }
}

// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s52sync_parameter_isolated_witness_of_async_requirement1SVAA1PA2aDP4test9isolationyScA_pSgYi_tYaFTW : $@convention(witness_method: P) @async (@sil_isolated @guaranteed Optional<any Actor>, @in_guaranteed S) -> () {
// CHECK: bb0([[ISOLATION:%.*]] : @guaranteed $Optional<any Actor>, [[SELF:%.*]] : $*S):
// CHECK:  hop_to_executor [[ISOLATION]]
// CHECK:  [[WITNESS:%.*]] = function_ref @$s52sync_parameter_isolated_witness_of_async_requirement1SV4test9isolationyScA_pSgYi_tF : $@convention(method) (@sil_isolated @guaranteed Optional<any Actor>, S) -> ()
// CHECK:  apply [[WITNESS]]([[ISOLATION]], {{.*}}) : $@convention(method) (@sil_isolated @guaranteed Optional<any Actor>, S) -> ()
// CHECK: } // end sil function '$s52sync_parameter_isolated_witness_of_async_requirement1SVAA1PA2aDP4test9isolationyScA_pSgYi_tYaFTW'
