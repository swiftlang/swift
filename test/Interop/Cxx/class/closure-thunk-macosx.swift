// RUN: %target-swiftxx-frontend -I %S/Inputs -Xllvm -sil-print-types -emit-silgen %s | %FileCheck %s

// REQUIRES: OS=macosx

import Closure

// CHECK: sil [ossa] @$s4main20testClosureToFuncPtryyF : $@convention(thin) () -> () {
// CHECK: %[[V0:.*]] = function_ref @$s4main20testClosureToFuncPtryyFySo9ARCStrongVcfU_To : $@convention(c) (@owned ARCStrong) -> ()
// CHECK: %[[V1:.*]] = function_ref @_Z14cfuncARCStrongPFv9ARCStrongE : $@convention(c) (@convention(c) (@owned ARCStrong) -> ()) -> ()
// CHECK: apply %[[V1]](%[[V0]]) : $@convention(c) (@convention(c) (@owned ARCStrong) -> ()) -> ()

// CHECK: sil private [thunk] [ossa] @$s4main20testClosureToFuncPtryyFySo9ARCStrongVcfU_To : $@convention(c) (@owned ARCStrong) -> () {
// CHECK: bb0(%[[V0:.*]] : @owned $ARCStrong):
// CHECK: %[[V1:.*]] = begin_borrow %[[V0]] : $ARCStrong
// CHECK: %[[V2:.*]] = function_ref @$s4main20testClosureToFuncPtryyFySo9ARCStrongVcfU_ : $@convention(thin) (@guaranteed ARCStrong) -> ()
// CHECK: apply %[[V2]](%[[V1]]) : $@convention(thin) (@guaranteed ARCStrong) -> ()
// CHECK: end_borrow %[[V1]] : $ARCStrong
// CHECK: destroy_value %[[V0]] : $ARCStrong

public func testClosureToFuncPtr() {
 cfuncARCStrong({N in})
}

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSo10NonTrivialVIegr_ABIeyBr_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed () -> @out NonTrivial) -> @out NonTrivial {
// CHECK: bb0(%[[V0:.*]] : $*NonTrivial, %[[V1:.*]] : $*@block_storage @callee_guaranteed () -> @out NonTrivial):
// CHECK: %[[V2:.*]] = project_block_storage %[[V1]] : $*@block_storage @callee_guaranteed () -> @out NonTrivial
// CHECK: %[[V3:.*]] = load [copy] %[[V2]] : $*@callee_guaranteed () -> @out NonTrivial
// CHECK: %[[V4:.*]] = begin_borrow %[[V3]] : $@callee_guaranteed () -> @out NonTrivial
// CHECK: apply %[[V4]](%[[V0]]) : $@callee_guaranteed () -> @out NonTrivial
// CHECK: end_borrow %[[V4]] : $@callee_guaranteed () -> @out NonTrivial
// CHECK: %[[V8:.*]] = tuple ()
// CHECK: destroy_value %[[V3]] : $@callee_guaranteed () -> @out NonTrivial
// CHECK: return %[[V8]] : $()

public func testClosureToBlockReturnNonTrivial() {
  cfuncReturnNonTrivial({() -> NonTrivial in return NonTrivial() })
}
