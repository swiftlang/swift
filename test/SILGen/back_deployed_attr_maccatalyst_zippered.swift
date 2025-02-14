// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target x86_64-apple-macosx10.15 -target-variant x86_64-apple-ios13.1-macabi -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target x86_64-apple-macosx10.15 -target-variant x86_64-apple-ios13.1-macabi | %FileCheck %s

// REQUIRES: OS=macosx || OS=maccatalyst

// -- Fallback definition of trivialFunc_iOS()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy15trivialFunc_iOSyyFTwB : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc_iOS()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy15trivialFunc_iOSyyFTwb : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK:   [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss33_stdlib_isVariantOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy15trivialFunc_iOSyyFTwB : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy15trivialFunc_iOSyyF : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[ORIGFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of trivialFunc_iOS()
// CHECK-LABEL: sil [ossa] @$s11back_deploy15trivialFunc_iOSyyF : $@convention(thin) () -> ()
@backDeployed(before: iOS 51.1)
public func trivialFunc_iOS() {}

// -- Fallback definition of trivialFunc_macOS()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy17trivialFunc_macOSyyFTwB : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc_macOS()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy17trivialFunc_macOSyyFTwb : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK:   [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 53
// CHECK:   [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy17trivialFunc_macOSyyFTwB : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy17trivialFunc_macOSyyF : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[ORIGFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of trivialFunc_macOS()
// CHECK-LABEL: sil [available 10.53] [ossa] @$s11back_deploy17trivialFunc_macOSyyF : $@convention(thin) () -> ()
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
// CHECK:   [[MACOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 10
// CHECK:   [[MACOS_MINOR:%.*]] = integer_literal $Builtin.Word, 53
// CHECK:   [[MACOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[IOS_MAJOR:%.*]] = integer_literal $Builtin.Word, 51
// CHECK:   [[IOS_MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[IOS_PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss042_stdlib_isOSVersionAtLeastOrVariantVersiondE0yBi1_Bw_BwBwBwBwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MACOS_MAJOR]], [[MACOS_MINOR]], [[MACOS_PATCH]], [[IOS_MAJOR]], [[IOS_MINOR]], [[IOS_PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
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
// CHECK-LABEL: sil [available 10.53] [ossa] @$s11back_deploy019trivialFunc_iOS_macE0yyF : $@convention(thin) () -> ()
@backDeployed(before: iOS 51.1, macOS 10.53)
public func trivialFunc_iOS_macOS() {}

// -- Original definition of trivialFunc_watchOS()
// CHECK-LABEL: sil [ossa] @$s11back_deploy19trivialFunc_watchOSyyF : $@convention(thin) () -> ()
@backDeployed(before: watchOS 43.2)
public func trivialFunc_watchOS() {}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyF : $@convention(thin) () -> ()
func caller() {
  // -- Verify the thunk is not called
  // These functions are not back deployed on macOS so they should be called directly.
  // CHECK: {{%.*}} = function_ref @$s11back_deploy15trivialFunc_iOSyyF : $@convention(thin) () -> ()
  trivialFunc_iOS()
  // CHECK: {{%.*}} = function_ref @$s11back_deploy19trivialFunc_watchOSyyF : $@convention(thin) () -> ()
  trivialFunc_watchOS()

  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy17trivialFunc_macOSyyFTwb : $@convention(thin) () -> ()
  trivialFunc_macOS()
  // CHECK: {{%.*}} = function_ref @$s11back_deploy019trivialFunc_iOS_macE0yyFTwb : $@convention(thin) () -> ()
  trivialFunc_iOS_macOS()
}
