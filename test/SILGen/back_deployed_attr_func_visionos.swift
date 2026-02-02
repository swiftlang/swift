// RUN: %target-swift-emit-sil -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-xros1.0 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-xros1.0 | %FileCheck %s

// REQUIRES: OS=xros

// -- Fallback definition of trivialFunc()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy11trivialFuncyyFTwB : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy11trivialFuncyyFTwb : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 2
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy11trivialFuncyyFTwB : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy11trivialFuncyyF : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[ORIGFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of trivialFunc()
// CHECK-LABEL: sil [available 2] [ossa] @$s11back_deploy11trivialFuncyyF : $@convention(thin) () -> ()
@backDeployed(before: visionOS 2)
public func trivialFunc() {}

// -- Fallback definition of trivialFunc_iOS()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy15trivialFunc_iOSyyFTwB : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc_iOS()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy15trivialFunc_iOSyyFTwb : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
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
// CHECK-LABEL: sil [available 1.1] [ossa] @$s11back_deploy15trivialFunc_iOSyyF : $@convention(thin) () -> ()
@backDeployed(before: iOS 17.4)
public func trivialFunc_iOS() {}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyF : $@convention(thin) () -> ()
func caller() {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy11trivialFuncyyFTwb : $@convention(thin) () -> ()
  trivialFunc()
  // CHECK: {{%.*}} = function_ref @$s11back_deploy15trivialFunc_iOSyyFTwb : $@convention(thin) () -> ()
  trivialFunc_iOS()
}
