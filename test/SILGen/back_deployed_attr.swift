// RUN: %target-swift-emit-sil -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s --check-prefixes=CHECK,CHECK-BACK-DEPLOY
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.52 | %FileCheck %s --check-prefixes=CHECK,CHECK-NATIVE
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.60 | %FileCheck %s --check-prefixes=CHECK,CHECK-NATIVE

// REQUIRES: OS=macosx

@_backDeploy(before: macOS 10.52)
public func someFunc() {}

public struct S<T> {
  @usableFromInline var _x: T

  @_backDeploy(before: macOS 10.52)
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
  // CHECK: function_ref @$s11back_deploy1SV1xxvgTwb : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @out τ_0_0
  _ = s.x
  // CHECK: function_ref @$s11back_deploy1SV1xxvsTwb : $@convention(method) <τ_0_0> (@in τ_0_0, @inout S<τ_0_0>) -> ()
  s.x = Z()
}

// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy0A14DeployedCalleryyAA1SVyAA1ZVGzFTwB
@_backDeploy(before: macOS 10.52)
public func backDeployedCaller(_ s: inout S<Z>) {
  // CHECK: function_ref @$s11back_deploy8someFuncyyFTwb : $@convention(thin) () -> ()
  someFunc()
  // CHECK: function_ref @$s11back_deploy1SV1xxvgTwb : $@convention(method) <τ_0_0> (@in_guaranteed S<τ_0_0>) -> @out τ_0_0
  _ = s.x
  // CHECK: function_ref @$s11back_deploy1SV1xxvsTwb : $@convention(method) <τ_0_0> (@in τ_0_0, @inout S<τ_0_0>) -> ()
  s.x = Z()
}
