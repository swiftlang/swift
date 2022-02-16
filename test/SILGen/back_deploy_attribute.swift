// RUN: %target-swift-emit-sil %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.60 | %FileCheck %s
// REQUIRES: OS=macosx

// ---- Back deployment thunk for backDeployedFunc()
// CHECK-LABEL: sil non_abi [serialized] [thunk] [available 10.51] [ossa] @$s21back_deploy_attribute0A12DeployedFuncyyFTw : $@convention(thin) () -> ()
// CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK: cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK: br [[RETURN_BB:bb.*]](undef : $())
//
// CHECK: [[AVAIL_BB]]:
// CHECK: [[ORIGFN:%.*]] = function_ref @$s21back_deploy_attribute0A12DeployedFuncyyF : $@convention(thin) () -> ()
// CHECK: [[RESULT:%.*]] = apply [[ORIGFN]]() : $@convention(thin) () -> ()
// CHECK: br [[RESULT_BB:bb[0-9]+]]([[RESULT]] : $())
//
// CHECK: [[RESULT_BB]]([[RESULT_BB_ARG:%.*]] : $()):
// CHECK: br [[RETURN_BB]]([[RESULT_BB_ARG]] : $())
//
// CHECK: [[RETURN_BB]]([[RETURN_BB_ARG:%.*]] : $())
// CHECK: return [[RETURN_BB_ARG]] : $()

// ---- Original definition of backDeployedFunc()
// CHECK-LABEL: sil [serialized] [available 10.51] [ossa] @$s21back_deploy_attribute0A12DeployedFuncyyF : $@convention(thin) () -> ()
@available(macOS 10.51, *)
@_backDeploy(macOS 10.50)
public func backDeployedFunc() {}

@available(macOS 10.50, *)
public struct TopLevelStruct {
  // ---- Back deployment thunk for TopLevelStruct.backDeployedStructFunc()
  // CHECK-LABEL: sil non_abi [serialized] [thunk] [available 10.51] [ossa] @$s21back_deploy_attribute14TopLevelStructV0a8DeployedF4FuncyyFTw : $@convention(method) (TopLevelStruct) -> ()
  // CHECK: bb0([[BB0_ARG:%.*]] : $TopLevelStruct):
  // CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
  // CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 51
  // CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK: [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[UNAVAIL_BB]]:
  // CHECK: br [[RETURN_BB:bb.*]](undef : $())
  //
  // CHECK: [[AVAIL_BB]]:
  // CHECK: [[ORIGFN:%.*]] = function_ref @$s21back_deploy_attribute14TopLevelStructV0a8DeployedF4FuncyyF : $@convention(method) (TopLevelStruct) -> ()
  // CHECK: [[RESULT:%.*]] = apply [[ORIGFN]]([[BB0_ARG]]) : $@convention(method) (TopLevelStruct) -> ()
  // CHECK: br [[RESULT_BB:bb[0-9]+]]([[RESULT]] : $())
  //
  // CHECK: [[RESULT_BB]]([[RESULT_BB_ARG:%.*]] : $()):
  // CHECK: br [[RETURN_BB]]([[RESULT_BB_ARG]] : $())
  //
  // CHECK: [[RETURN_BB]]([[RETURN_BB_ARG:%.*]] : $())
  // CHECK: return [[RETURN_BB_ARG]] : $()

  // ---- Original definition of TopLevelStruct.backDeployedStructFunc()
  // CHECK-LABEL: sil [serialized] [available 10.51] [ossa] @$s21back_deploy_attribute14TopLevelStructV0a8DeployedF4FuncyyF : $@convention(method) (TopLevelStruct) -> ()
  @available(macOS 10.51, *)
  @_backDeploy(macOS 10.50)
  public func backDeployedStructFunc() {}
}

// CHECK-LABEL: sil hidden [available 10.51] [ossa] @$s21back_deploy_attribute6calleryyAA14TopLevelStructVF : $@convention(thin) (TopLevelStruct) -> ()
@available(macOS 10.51, *)
func caller(_ s: TopLevelStruct) {
  // CHECK: bb0([[STRUCT_ARG:%.*]] : $TopLevelStruct):
  // CHECK: [[FNREF:%.*]] = function_ref @$s21back_deploy_attribute0A12DeployedFuncyyFTw : $@convention(thin) () -> ()
  // CHECK: [[FNRES:%.*]] = apply [[FNREF]]() : $@convention(thin) () -> ()
  backDeployedFunc()
  // CHECK: [[FNREF:%.*]] = function_ref @$s21back_deploy_attribute14TopLevelStructV0a8DeployedF4FuncyyFTw : $@convention(method) (TopLevelStruct) -> ()
  // CHECK: [[FNRES:%.*]] = apply [[FNREF]]([[STRUCT_ARG]]) : $@convention(method) (TopLevelStruct) -> ()
  s.backDeployedStructFunc()
}
