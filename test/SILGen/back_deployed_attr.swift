// RUN: %target-swift-emit-sil -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s --check-prefixes=CHECK,CHECK-BACK-DEPLOY
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.52 | %FileCheck %s --check-prefixes=CHECK,CHECK-NATIVE
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.60 | %FileCheck %s --check-prefixes=CHECK,CHECK-NATIVE

// REQUIRES: OS=macosx

// CHECK: sil non_abi [serialized] [ossa] @$s11back_deploy8someFuncyyFTwB
// CHECK: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy8someFuncyyFTwb
// CHECK: sil [available 10.52] [ossa] @$s11back_deploy8someFuncyyF
@backDeployed(before: macOS 10.52)
public func someFunc() {}

// CHECK-NOT: @$s11back_deploy0A13DeployedOniOSyyFTwB
// CHECK-NOT: @$s11back_deploy0A13DeployedOniOSyyFTwb
// CHECK: sil [ossa] @$s11back_deploy0A13DeployedOniOSyyF
@backDeployed(before: iOS 13.13)
public func backDeployedOniOS() {}

public struct S<T> {
  @usableFromInline var _x: T

  @backDeployed(before: macOS 10.52)
  public var x: T {
    get { _x }
    set { _x = newValue }
  }
}

public struct Z {
  public init() {}
}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy15resilientCalleryyAA1SVyAA1ZVGzF
func resilientCaller(_ s: inout S<Z>) {
  // CHECK-BACK-DEPLOY: function_ref @$s11back_deploy8someFuncyyFTwb : $@convention(thin) () -> ()
  // CHECK-NATIVE: function_ref @$s11back_deploy8someFuncyyF : $@convention(thin) () -> ()
  someFunc()
  // CHECK: function_ref @$s11back_deploy0A13DeployedOniOSyyF : $@convention(thin) () -> ()
  backDeployedOniOS()
  // CHECK-BACK-DEPLOY: function_ref @$s11back_deploy1SV1xxvgTwb : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @out τ_0_0
  // CHECK-NATIVE: function_ref @$s11back_deploy1SV1xxvg : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @out τ_0_0
  _ = s.x
  // CHECK-BACK-DEPLOY: function_ref @$s11back_deploy1SV1xxvsTwb : $@convention(method) <τ_0_0> (@in τ_0_0, @inout S<τ_0_0>) -> ()
  // CHECK-NATIVE: function_ref @$s11back_deploy1SV1xxvs : $@convention(method) <τ_0_0> (@in τ_0_0, @inout S<τ_0_0>) -> ()
  s.x = Z()
}

// CHECK-LABEL: sil [serialized] [ossa] @$s11back_deploy15inlinableCalleryyAA1SVyAA1ZVGzF
@inlinable
func inlinableCaller(_ s: inout S<Z>) {
  // CHECK: function_ref @$s11back_deploy8someFuncyyFTwb : $@convention(thin) () -> ()
  someFunc()
  // CHECK: function_ref @$s11back_deploy0A13DeployedOniOSyyF : $@convention(thin) () -> ()
  backDeployedOniOS()
  // CHECK: function_ref @$s11back_deploy1SV1xxvgTwb : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @out τ_0_0
  _ = s.x
  // CHECK: function_ref @$s11back_deploy1SV1xxvsTwb : $@convention(method) <τ_0_0> (@in τ_0_0, @inout S<τ_0_0>) -> ()
  s.x = Z()
}

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy10aeicCalleryyAA1SVyAA1ZVGzF
@_alwaysEmitIntoClient
func aeicCaller(_ s: inout S<Z>) {
  // CHECK: function_ref @$s11back_deploy8someFuncyyFTwb : $@convention(thin) () -> ()
  someFunc()
  // CHECK: function_ref @$s11back_deploy0A13DeployedOniOSyyF : $@convention(thin) () -> ()
  backDeployedOniOS()
  // CHECK: function_ref @$s11back_deploy1SV1xxvgTwb : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @out τ_0_0
  _ = s.x
  // CHECK: function_ref @$s11back_deploy1SV1xxvsTwb : $@convention(method) <τ_0_0> (@in τ_0_0, @inout S<τ_0_0>) -> ()
  s.x = Z()
}

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy0A14DeployedCalleryyAA1SVyAA1ZVGzFTwB
@backDeployed(before: macOS 10.52)
public func backDeployedCaller(_ s: inout S<Z>) {
  // CHECK: function_ref @$s11back_deploy8someFuncyyFTwb : $@convention(thin) () -> ()
  someFunc()
  // CHECK: function_ref @$s11back_deploy0A13DeployedOniOSyyF : $@convention(thin) () -> ()
  backDeployedOniOS()
  // CHECK: function_ref @$s11back_deploy1SV1xxvgTwb : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @out τ_0_0
  _ = s.x
  // CHECK: function_ref @$s11back_deploy1SV1xxvsTwb : $@convention(method) <τ_0_0> (@in τ_0_0, @inout S<τ_0_0>) -> ()
  s.x = Z()
}

// The same bug from test/Concurrency/nonisolated_nonsending.swift also applied to
// back-deployment thunks.

// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy0A29DeployedNonisolatedNonsendingSiyYaFTwb :
@backDeployed(before: macOS 10.52)
nonisolated(nonsending)
public func backDeployedNonisolatedNonsending() async -> Int {
  // CHECK: bb0(%0 : @guaranteed $Optional<any Actor>):
  // CHECK:   [[FALLBACK_FN:%.*]] = function_ref @$s11back_deploy0A29DeployedNonisolatedNonsendingSiyYaFTwB :
  // CHECK:   apply [[FALLBACK_FN]](%0)
  // CHECK:   [[SHIPPING_FN:%.*]] = function_ref @$s11back_deploy0A29DeployedNonisolatedNonsendingSiyYaF :
  // CHECK:   apply [[SHIPPING_FN]](%0)
  return 0
}

// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy0A32DeployedBeforeVersionMappingTo26SiyFTwb :
@backDeployed(before: macOS 16)
public func backDeployedBeforeVersionMappingTo26() -> Int {
  // CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 26
  // CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK: [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: cond_br [[AVAIL]]
  return 0
}
