// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: concurrency

@available(macOS 51.0, *)
@usableFromInline func otherFunc() async {}

// -- Fallback definition of asyncFunc()
// CHECK: sil non_abi [serialized] [ossa] @$s11back_deploy9asyncFuncyyYaFTwB : $@convention(thin) @async () -> ()
// CHECK: bb0:
// CHECK:   [[FNREF:%.*]] = function_ref @$s11back_deploy9otherFuncyyYaF : $@convention(thin) @async () -> ()
// CHECK:   [[APPLY:%.*]] = apply [[FNREF]]() : $@convention(thin) @async () -> ()
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy9asyncFuncyyYaFTwb : $@convention(thin) @async () -> ()
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy9asyncFuncyyYaFTwB : $@convention(thin) @async () -> ()
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]() : $@convention(thin) @async () -> ()
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy9asyncFuncyyYaF : $@convention(thin) @async () -> ()
// CHECK:   {{%.*}} = apply [[ORIGFN]]() : $@convention(thin) @async () -> ()
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of trivialFunc()
// CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy9asyncFuncyyYaF : $@convention(thin) @async () -> ()
@available(macOS 51.0, *)
@backDeployed(before: macOS 52.1)
public func asyncFunc() async {
  await otherFunc()
}

// CHECK-LABEL: sil hidden [available 51.0] [ossa] @$s11back_deploy6calleryyYaF : $@convention(thin) @async () -> ()
@available(macOS 51.0, *)
func caller() async {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy9asyncFuncyyYaFTwb : $@convention(thin) @async () -> ()
  await asyncFunc()
}

