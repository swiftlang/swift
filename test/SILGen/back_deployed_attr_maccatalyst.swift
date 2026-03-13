// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-ios13.1-macabi -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-ios13.1-macabi | %FileCheck %s

// REQUIRES: OS=macosx || OS=maccatalyst

// -- Original definition of trivialFunc_macOS()
// CHECK-LABEL: sil [ossa] @$s11back_deploy17trivialFunc_macOSyyF : $@convention(thin) () -> ()
@backDeployed(before: macOS 10.53)
public func trivialFunc_macOS() {}

// -- Fallback definition of trivialFunc_iOS_macOS()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy019trivialFunc_iOS_macE0yyFTwB : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc_iOS_macOS()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy019trivialFunc_iOS_macE0yyFTwb : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy019trivialFunc_iOS_macE0yyFTwB : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy019trivialFunc_iOS_macE0yyF : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[ORIGFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of trivialFunc_iOS_macOS()
// CHECK-LABEL: sil [available 51.1] [ossa] @$s11back_deploy019trivialFunc_iOS_macE0yyF : $@convention(thin) () -> ()
@backDeployed(before: iOS 51.1, macOS 10.53)
public func trivialFunc_iOS_macOS() {}

// -- Fallback definition of trivialFunc_iOS_macOS_macCatalyst()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy019trivialFunc_iOS_mace1_F8CatalystyyFTwB : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc_iOS_macOS_macCatalyst()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy019trivialFunc_iOS_mace1_F8CatalystyyFTwb : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 53
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 2
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy019trivialFunc_iOS_mace1_F8CatalystyyFTwB : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy019trivialFunc_iOS_mace1_F8CatalystyyF : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[ORIGFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of trivialFunc_iOS_macOS_macCatalyst()
// CHECK-LABEL: sil [available 53.2] [ossa] @$s11back_deploy019trivialFunc_iOS_mace1_F8CatalystyyF : $@convention(thin) () -> ()
@backDeployed(before: iOS 51.1, macOS 10.53, macCatalyst 53.2)
public func trivialFunc_iOS_macOS_macCatalyst() {}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyF : $@convention(thin) () -> ()
func caller() {
  // -- Verify the thunk is not called
  // The function is not back deployed on iOS so it should be called directly.
  // CHECK: {{%.*}} = function_ref @$s11back_deploy17trivialFunc_macOSyyF : $@convention(thin) () -> ()
  trivialFunc_macOS()

  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy019trivialFunc_iOS_macE0yyFTwb : $@convention(thin) () -> ()
  trivialFunc_iOS_macOS()
  // CHECK: {{%.*}} = function_ref @$s11back_deploy019trivialFunc_iOS_mace1_F8CatalystyyFTwb : $@convention(thin) () -> ()
  trivialFunc_iOS_macOS_macCatalyst()
}
