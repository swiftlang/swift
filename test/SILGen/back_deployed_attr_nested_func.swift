// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s

// REQUIRES: OS=macosx

// -- Fallback definition of hasNestedFunc() references the nested function.
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy13hasNestedFuncSiyFTwB : $@convention(thin) () -> Int
// CHECK: bb0:
// CHECK:   [[NESTEDFN:%.*]] = function_ref @$s11back_deploy13hasNestedFuncSiyF6nestedL_SiyF : $@convention(thin) () -> Int
// CHECK:   [[RESULT:%.*]] = apply [[NESTEDFN]]() : $@convention(thin) () -> Int
// CHECK:   return [[RESULT]] : $Int

// -- The nested function
// CHECK-LABEL: sil shared [serialized] [ossa] @$s11back_deploy13hasNestedFuncSiyF6nestedL_SiyF : $@convention(thin) () -> Int

// -- Back deployment thunk for hasNestedFunc()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy13hasNestedFuncSiyFTwb : $@convention(thin) () -> Int
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy13hasNestedFuncSiyFTwB : $@convention(thin) () -> Int
// CHECK:   [[RESULT:%.*]] = apply [[FALLBACKFN]]() : $@convention(thin) () -> Int
// CHECK:   br [[RETURN_BB:bb[0-9]+]]([[RESULT]] : $Int)
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy13hasNestedFuncSiyF : $@convention(thin) () -> Int
// CHECK:   [[RESULT:%.*]] = apply [[ORIGFN]]() : $@convention(thin) () -> Int
// CHECK:   br [[RETURN_BB]]([[RESULT]] : $Int)
//
// CHECK: [[RETURN_BB]]([[RETURN_BB_ARG:%.*]] : $Int)
// CHECK:   return [[RETURN_BB_ARG]] : $Int

// -- Original definition of hasNestedFunc() references the same nested function.
// CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy13hasNestedFuncSiyF : $@convention(thin) () -> Int
// CHECK: bb0:
// CHECK:   [[NESTEDFN:%.*]] = function_ref @$s11back_deploy13hasNestedFuncSiyF6nestedL_SiyF : $@convention(thin) () -> Int
// CHECK:   [[RESULT:%.*]] = apply [[NESTEDFN]]() : $@convention(thin) () -> Int
// CHECK:   return [[RESULT]] : $Int
@backDeployed(before: macOS 52.1)
public func hasNestedFunc() -> Int {
  func nested() -> Int { 42 }
  return nested()
}

// -- Fallback definition of hasNestedClosure() references the closure.
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy16hasNestedClosureSiyFTwB : $@convention(thin) () -> Int
// CHECK: bb0:
// CHECK:   [[CLOSUREFN:%.*]] = function_ref @$s11back_deploy16hasNestedClosureSiyFSiycfU_ : $@convention(thin) () -> Int
// CHECK:   [[THICK:%.*]] = thin_to_thick_function [[CLOSUREFN]]
// CHECK:   return {{%.*}} : $Int

// -- The nested closure
// CHECK-LABEL: sil shared [serialized] [ossa] @$s11back_deploy16hasNestedClosureSiyFSiycfU_ : $@convention(thin) () -> Int

// -- Back deployment thunk for hasNestedClosure()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy16hasNestedClosureSiyFTwb : $@convention(thin) () -> Int
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy16hasNestedClosureSiyFTwB : $@convention(thin) () -> Int
// CHECK:   [[RESULT:%.*]] = apply [[FALLBACKFN]]() : $@convention(thin) () -> Int
// CHECK:   br [[RETURN_BB:bb[0-9]+]]([[RESULT]] : $Int)
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy16hasNestedClosureSiyF : $@convention(thin) () -> Int
// CHECK:   [[RESULT:%.*]] = apply [[ORIGFN]]() : $@convention(thin) () -> Int
// CHECK:   br [[RETURN_BB]]([[RESULT]] : $Int)
//
// CHECK: [[RETURN_BB]]([[RETURN_BB_ARG:%.*]] : $Int)
// CHECK:   return [[RETURN_BB_ARG]] : $Int

// -- Original definition of hasNestedClosure() references the same closure.
// CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy16hasNestedClosureSiyF : $@convention(thin) () -> Int
// CHECK: bb0:
// CHECK:   [[CLOSUREFN:%.*]] = function_ref @$s11back_deploy16hasNestedClosureSiyFSiycfU_ : $@convention(thin) () -> Int
// CHECK:   [[THICK:%.*]] = thin_to_thick_function [[CLOSUREFN]]
// CHECK:   return {{%.*}} : $Int
@backDeployed(before: macOS 52.1)
public func hasNestedClosure() -> Int {
  let c = { () -> Int in 42 }
  return c()
}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyF : $@convention(thin) () -> ()
func caller() {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy13hasNestedFuncSiyFTwb : $@convention(thin) () -> Int
  _ = hasNestedFunc()
  // CHECK: {{%.*}} = function_ref @$s11back_deploy16hasNestedClosureSiyFTwb : $@convention(thin) () -> Int
  _ = hasNestedClosure()
}
