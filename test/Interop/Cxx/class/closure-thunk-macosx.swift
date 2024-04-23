// RUN: %target-swiftxx-frontend -I %S/Inputs -emit-sil %s | %FileCheck %s

// REQUIRES: OS=macosx

import Closure

// CHECK: sil @$s4main25testClosureToBlockARCWeakyyF : $@convention(thin) () -> () {
// CHECK: %[[V0:.*]] = function_ref @$s4main25testClosureToBlockARCWeakyyFySo0F0VcfU_ : $@convention(thin) (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V1:.*]] = thin_to_thick_function %[[V0]] : $@convention(thin) (@in_guaranteed ARCWeak) -> () to $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V2:.*]] = alloc_stack $@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V3:.*]] = project_block_storage %[[V2]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: store %[[V1]] to %[[V3]] : $*@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V7:.*]] = function_ref @$sSo7ARCWeakVIegn_ABIeyBi_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), @in ARCWeak) -> ()
// CHECK: %[[V6:.*]] = init_block_storage_header %[[V2]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), invoke %[[V7]] : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), @in ARCWeak) -> (), type $@convention(block) (@in ARCWeak) -> ()
// CHECK: %[[V8:.*]] = copy_block %[[V6]] : $@convention(block) (@in ARCWeak) -> ()
// CHECK: dealloc_stack %[[V2]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V11:.*]] = function_ref @_Z12cfuncARCWeakU13block_pointerFv7ARCWeakE : $@convention(c) (@convention(block) (@in ARCWeak) -> ()) -> ()
// CHECK: apply %[[V11]](%[[V8]]) : $@convention(c) (@convention(block) (@in ARCWeak) -> ()) -> ()
// CHECK: strong_release %[[V8]] : $@convention(block) (@in ARCWeak) -> ()
// CHECK: %[[V12:.*]] = tuple ()
// CHECK: return %[[V12]] : $()

// CHECK: sil shared [transparent] [reabstraction_thunk] @$sSo7ARCWeakVIegn_ABIeyBi_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), @in ARCWeak) -> () {
// CHECK: bb0(%[[V0:.*]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), %[[V1:.*]] : $*ARCWeak):
// CHECK: %[[V2:.*]] = project_block_storage %[[V0]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V3:.*]] = load %[[V2]] : $*@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: strong_retain %[[V3]] : $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: apply %[[V3]](%[[V1]]) : $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V6:.*]] = tuple ()
// CHECK: destroy_addr %[[V1]] : $*ARCWeak
// CHECK: strong_release %[[V3]] : $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: return %[[V6]] : $()

// ARCWeak is destroyed by the callee.
public func testClosureToBlockARCWeak() {
  cfuncARCWeak({ARCWeak in})
}

// CHECK: sil @$s4main20testClosureToFuncPtryyF : $@convention(thin) () -> () {
// CHECK: %[[V0:.*]] = function_ref @$s4main20testClosureToFuncPtryyFySo9ARCStrongVcfU_To : $@convention(c) (@owned ARCStrong) -> ()
// CHECK: %[[V1:.*]] = function_ref @_Z14cfuncARCStrongPFv9ARCStrongE : $@convention(c) (@convention(c) (@owned ARCStrong) -> ()) -> ()
// CHECK: apply %[[V1]](%[[V0]]) : $@convention(c) (@convention(c) (@owned ARCStrong) -> ()) -> ()

// CHECK: sil private [thunk] @$s4main20testClosureToFuncPtryyFySo9ARCStrongVcfU_To : $@convention(c) (@owned ARCStrong) -> () {
// CHECK: bb0(%[[V0:.*]] : $ARCStrong):
// CHECK: %[[V1:.*]] = function_ref @$s4main20testClosureToFuncPtryyFySo9ARCStrongVcfU_ : $@convention(thin) (@guaranteed ARCStrong) -> ()
// CHECK: apply %[[V1]](%[[V0]]) : $@convention(thin) (@guaranteed ARCStrong) -> ()
// CHECK: release_value %[[V0]] : $ARCStrong
// CHECK: return %[[V2]] : $()

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
