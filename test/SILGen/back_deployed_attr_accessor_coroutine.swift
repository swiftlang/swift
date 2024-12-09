// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s

// REQUIRES: OS=macosx

public struct TopLevelStruct {
  // -- Fallback definition for TopLevelStruct.property.read
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy14TopLevelStructV8propertyACvrTwB : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  // CHECK: bb0([[BB0_ARG:%.*]] : $TopLevelStruct):
  // CHECK:   yield [[BB0_ARG]] : $TopLevelStruct, resume [[RESUME_BB:bb[0-9]+]], unwind [[UNWIND_BB:bb[0-9]+]]
  //
  // CHECK: [[RESUME_BB]]:
  // CHECK:   [[RESULT:%.*]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  //
  // CHECK: [[UNWIND_BB]]:
  // CHECK:   unwind

  // -- Back deployment thunk for TopLevelStruct.property.read
  // CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy14TopLevelStructV8propertyACvrTwb : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  // CHECK: bb0([[BB0_ARG:%.*]] : $TopLevelStruct):
  // CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
  // CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
  // CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[UNAVAIL_BB]]:
  // CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy14TopLevelStructV8propertyACvrTwB : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  // CHECK:   ([[YIELD_RES:%.*]], [[YIELD_TOK:%.*]]) = begin_apply [[FALLBACKFN]]([[BB0_ARG]]) : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  // CHECK:   yield [[YIELD_RES]] : $TopLevelStruct, resume [[UNAVAIL_RESUME_BB:bb[0-9]+]], unwind [[UNAVAIL_UNWIND_BB:bb[0-9]+]]
  //
  // CHECK: [[UNAVAIL_UNWIND_BB]]:
  // CHECK:   end_apply [[YIELD_TOK]]
  // CHECK:   br [[UNWIND_BB:bb[0-9]+]]
  //
  // CHECK: [[UNAVAIL_RESUME_BB]]:
  // CHECK:   end_apply [[YIELD_TOK]]
  // CHECK:   br [[RETURN_BB:bb[0-9]+]]
  //
  // CHECK: [[AVAIL_BB]]:
  // CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy14TopLevelStructV8propertyACvr : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  // CHECK:   ([[YIELD_RES:%.*]], [[YIELD_TOK:%.*]]) = begin_apply [[ORIGFN]]([[BB0_ARG]]) : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  // CHECK:   yield [[YIELD_RES]] : $TopLevelStruct, resume [[AVAIL_RESUME_BB:bb[0-9]+]], unwind [[UAVAIL_UNWIND_BB:bb[0-9]+]]
  //
  // CHECK: [[UAVAIL_UNWIND_BB]]:
  // CHECK:   end_apply [[YIELD_TOK]]
  // CHECK:   br [[UNWIND_BB]]
  //
  // CHECK: [[AVAIL_RESUME_BB]]:
  // CHECK:   end_apply [[YIELD_TOK]]
  // CHECK:   br [[RETURN_BB]]
  //
  // CHECK: [[RETURN_BB]]:
  // CHECK:   [[RESULT:%.*]] = tuple ()
  // CHECK:   return [[RESULT]] : $()
  //
  // CHECK: [[UNWIND_BB]]:
  // CHECK:   unwind

  // -- Original definition of TopLevelStruct.property.read
  // CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy14TopLevelStructV8propertyACvr : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  @backDeployed(before: macOS 52.1)
  public var property: TopLevelStruct {
    _read { yield self }
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyAA14TopLevelStructVF : $@convention(thin) (TopLevelStruct) -> ()
// CHECK: bb0([[STRUCT_ARG:%.*]] : $TopLevelStruct):
func caller(_ s: TopLevelStruct) {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy14TopLevelStructV8propertyACvrTwb : $@yield_once @convention(method) (TopLevelStruct) -> @yields TopLevelStruct
  _ = s.property
}
