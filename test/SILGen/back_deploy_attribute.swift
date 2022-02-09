// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -target %target-cpu-apple-macosx10.60 | %FileCheck %s
// REQUIRES: OS=macosx

// CHECK-LABEL: sil [serialized] [available 10.51] [ossa] @$s21back_deploy_attribute0A12DeployedFuncyyF : $@convention(thin) () -> ()
@available(macOS 10.51, *)
@_backDeploy(macOS 10.50)
public func backDeployedFunc() {}

@available(macOS 10.50, *)
public struct TopLevelStruct {
  // CHECK-LABEL: sil [serialized] [available 10.51] [ossa] @$s21back_deploy_attribute14TopLevelStructV0a8DeployedF4FuncyyF : $@convention(method) (TopLevelStruct) -> ()
  @available(macOS 10.51, *)
  @_backDeploy(macOS 10.50)
  public func backDeployedStructFunc() {}
}


// FIXME(backDeploy): Verify SIL in a caller that requires back deployment


// CHECK-LABLEL: sil hidden [available 10.51] [ossa] @$s21back_deploy_attribute21alwaysAvailableCalleryyAA14TopLevelStructVF : $@convention(thin) (TopLevelStruct) -> ()
// CHECK: bb0(%0 : $TopLevelStruct):
@available(macOS 10.51, *)
func alwaysAvailableCaller(_ s: TopLevelStruct) {
  /// This function's availability meets the minimum availability of the APIs, so
  /// no back deployment logic is required.

  // CHECK: %2 = function_ref @$s21back_deploy_attribute0A12DeployedFuncyyF : $@convention(thin) () -> ()
  // CHECK: %3 = apply %2() : $@convention(thin) () -> ()
  backDeployedFunc()
  // CHECK: %4 = function_ref @$s21back_deploy_attribute14TopLevelStructV0a8DeployedF4FuncyyF : $@convention(method) (TopLevelStruct) -> ()
  // CHECK: %5 = apply %4(%0) : $@convention(method) (TopLevelStruct) -> ()
  s.backDeployedStructFunc()
}
