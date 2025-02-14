// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s

// REQUIRES: OS=macosx

public struct TopLevelStruct {
  // -- Fallback definition for TopLevelStruct.trivialMethod()
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy14TopLevelStructV13trivialMethodyyFTwB : $@convention(method) (TopLevelStruct) -> ()
  // CHECK: bb0({{%.*}} : $TopLevelStruct):
  // CHECK:   [[RESULT:%.*]] = tuple ()
  // CHECK:   return [[RESULT]] : $()

  // -- Back deployment thunk for TopLevelStruct.trivialMethod()
  // CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy14TopLevelStructV13trivialMethodyyFTwb : $@convention(method) (TopLevelStruct) -> ()
  // CHECK: bb0([[BB0_ARG:%.*]] : $TopLevelStruct):
  // CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
  // CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
  // CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[UNAVAIL_BB]]:
  // CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy14TopLevelStructV13trivialMethodyyFTwB : $@convention(method) (TopLevelStruct) -> ()
  // CHECK:   {{%.*}} = apply [[FALLBACKFN]]([[BB0_ARG]]) : $@convention(method) (TopLevelStruct) -> ()
  // CHECK:   br [[RETURN_BB:bb[0-9]+]]
  //
  // CHECK: [[AVAIL_BB]]:
  // CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy14TopLevelStructV13trivialMethodyyF : $@convention(method) (TopLevelStruct) -> ()
  // CHECK:   {{%.*}} = apply [[ORIGFN]]([[BB0_ARG]]) : $@convention(method) (TopLevelStruct) -> ()
  // CHECK:   br [[RETURN_BB]]
  //
  // CHECK: [[RETURN_BB]]
  // CHECK:   [[RESULT:%.*]] = tuple ()
  // CHECK:   return [[RESULT]] : $()

  // -- Original definition of TopLevelStruct.trivialMethod()
  // CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy14TopLevelStructV13trivialMethodyyF : $@convention(method) (TopLevelStruct) -> ()
  @backDeployed(before: macOS 52.1)
  public func trivialMethod() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyAA14TopLevelStructVF : $@convention(thin) (TopLevelStruct) -> ()
// CHECK: bb0([[STRUCT_ARG:%.*]] : $TopLevelStruct):
func caller(_ s: TopLevelStruct) {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy14TopLevelStructV13trivialMethodyyFTwb : $@convention(method) (TopLevelStruct) -> ()
  s.trivialMethod()
}
