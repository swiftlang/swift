// RUN: %target-swift-emit-sil -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s
// RUN: %target-swift-emit-silgen -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.60 | %FileCheck %s

// REQUIRES: OS=macosx

// -- Fallback definition of genericFunc()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy11genericFuncyxxlFTwB : $@convention(thin) <T> (@in_guaranteed T) -> @out T
// CHECK: bb0([[OUT_ARG:%.*]] : $*T, [[IN_ARG:%.*]] : $*T):
// CHECK:   copy_addr [[IN_ARG]] to [init] [[OUT_ARG]] : $*T
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for genericFunc()
// CHECK-LABEL: sil non_abi [serialized] [thunk] [ossa] @$s11back_deploy11genericFuncyxxlFTwb : $@convention(thin) <T> (@in_guaranteed T) -> @out T
// CHECK: bb0([[OUT_ARG:%.*]] : $*T, [[IN_ARG:%.*]] : $*T):
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy11genericFuncyxxlFTwB : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]<T>([[OUT_ARG]], [[IN_ARG]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy11genericFuncyxxlF : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   {{%.*}} = apply [[ORIGFN]]<T>([[OUT_ARG]], [[IN_ARG]]) : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of genericFunc()
// CHECK-LABEL: sil [available 10.52] [ossa] @$s11back_deploy11genericFuncyxxlF : $@convention(thin) <T> (@in_guaranteed T) -> @out T
@available(macOS 10.51, *)
@_backDeploy(before: macOS 10.52)
public func genericFunc<T>(_ t: T) -> T {
  return t
}

// CHECK-LABEL: sil hidden [available 10.51] [ossa] @$s11back_deploy6calleryyF : $@convention(thin) () -> ()
@available(macOS 10.51, *)
func caller() {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy11genericFuncyxxlFTwb : $@convention(thin) <τ_0_0> (@in_guaranteed τ_0_0) -> @out τ_0_0
  _ = genericFunc(Int32(1))
}
