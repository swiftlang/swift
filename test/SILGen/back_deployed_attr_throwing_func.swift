// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s

// REQUIRES: OS=macosx

// -- Fallback definition of throwingFunc()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy12throwingFuncyyKFTwB : $@convention(thin) () -> @error any Error
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for throwingFunc()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy12throwingFuncyyKFTwb : $@convention(thin) () -> @error any Error
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy12throwingFuncyyKFTwB : $@convention(thin) () -> @error any Error
// CHECK:   try_apply [[FALLBACKFN]]() : $@convention(thin) () -> @error any Error, normal [[UNAVAIL_NORMAL_BB:bb[0-9]+]], error [[UNAVAIL_ERROR_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_ERROR_BB]]([[ARG:%.*]] : @owned $any Error):
// CHECK:   br [[RETHROW_BB:bb[0-9]+]]([[ARG]] : $any Error)
//
// CHECK: [[UNAVAIL_NORMAL_BB]]([[ARG:%.*]] : $()):
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy12throwingFuncyyKF : $@convention(thin) () -> @error any Error
// CHECK:   try_apply [[ORIGFN]]() : $@convention(thin) () -> @error any Error, normal [[AVAIL_NORMAL_BB:bb[0-9]+]], error [[AVAIL_ERROR_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_ERROR_BB]]([[ARG:%.*]] : @owned $any Error):
// CHECK:   br [[RETHROW_BB]]([[ARG]] : $any Error)
//
// CHECK: [[AVAIL_NORMAL_BB]]([[ARG:%.*]] : $()):
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()
//
// CHECK: [[RETHROW_BB]]([[RETHROW_BB_ARG:%.*]] : @owned $any Error)
// CHECK:   throw [[RETHROW_BB_ARG]] : $any Error

// -- Original definition of throwingFunc()
// CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy12throwingFuncyyKF : $@convention(thin) () -> @error any Error
@backDeployed(before: macOS 52.1)
public func throwingFunc() throws {}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyKF : $@convention(thin) () -> @error any Error
func caller() throws {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy12throwingFuncyyKFTwb : $@convention(thin) () -> @error any Error
  try throwingFunc()
}
