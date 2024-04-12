// RUN: %target-swiftxx-frontend -I %S/Inputs -emit-silgen %s | %FileCheck %s

import NonTrivialClasses

// CHECK: sil [ossa] @$s4main14testNonTrivialyyF : $@convention(thin) () -> () {
// CHECK: %[[V7:.*]] = function_ref @$sSo10NonTrivialVIegn_ABIeyBc_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> (), @in_cxx NonTrivial) -> ()
// CHECK: %[[V6:.*]] = init_block_storage_header %[[V2:.*]] : $*@block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> (), invoke %[[V7]] : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> (), @in_cxx NonTrivial) -> (), type $@convention(block) (@in_cxx NonTrivial) -> ()
// CHECK: %[[V8:.*]] = copy_block %[[V6]] : $@convention(block) (@in_cxx NonTrivial) -> ()
// CHECK: dealloc_stack %[[V2]] : $*@block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK: %[[V11:.*]] = function_ref @_Z5cfuncU13block_pointerFv10NonTrivialE : $@convention(c) (@convention(block) (@in_cxx NonTrivial) -> ()) -> ()
// CHECK: apply %[[V11]](%[[V8]]) : $@convention(c) (@convention(block) (@in_cxx NonTrivial) -> ()) -> ()

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSo10NonTrivialVIegn_ABIeyBc_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> (), @in_cxx NonTrivial) -> () {
// CHECK: bb0(%[[V0:.*]] : $*@block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> (), %[[V1:.*]] : $*NonTrivial):
// CHECK-NEXT: %[[V2:.*]] = project_block_storage %[[V0]] : $*@block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK-NEXT: %[[V3:.*]] = load [copy] %[[V2]] : $*@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK-NEXT: %[[V4:.*]] = begin_borrow %[[V3]] : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK-NEXT: %[[V7:.*]] = apply %[[V4]](%[[V1]]) : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK-NEXT: end_borrow %[[V4]] : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK-NEXT: %[[V8:.*]] = tuple ()
// CHECK-NEXT: destroy_value %[[V3]] : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK-NEXT: return %[[V8]] : $()

// NonTrivial is destroyed by the caller.
public func testNonTrivial() {
  cfunc({NonTrivial in})
}

// CHECK: sil [ossa] @$s4main11testARCWeakyyF : $@convention(thin) () -> () {
// CHECK: %[[V7:.*]] = function_ref @$sSo7ARCWeakVIegn_ABIeyBi_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), @in ARCWeak) -> ()
// CHECK: %[[V6:.*]] = init_block_storage_header %[[V2:.*]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), invoke %[[V7]] : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), @in ARCWeak) -> (), type $@convention(block) (@in ARCWeak) -> ()
// CHECK: %[[V8:.*]] = copy_block %[[V6]] : $@convention(block) (@in ARCWeak) -> ()
// CHECK: dealloc_stack %[[V2]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V11:.*]] = function_ref @_Z12cfuncARCWeakU13block_pointerFv7ARCWeakE : $@convention(c) (@convention(block) (@in ARCWeak) -> ()) -> ()
// CHECK: apply %[[V11]](%[[V8]]) : $@convention(c) (@convention(block) (@in ARCWeak) -> ()) -> ()

// CHECK: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSo7ARCWeakVIegn_ABIeyBi_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), @in ARCWeak) -> () {
// CHECK: bb0(%[[V0:.*]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> (), %[[V1:.*]] : $*ARCWeak):
// CHECK: %[[V2:.*]] = project_block_storage %[[V0]] : $*@block_storage @callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V3:.*]] = load [copy] %[[V2]] : $*@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V4:.*]] = begin_borrow %[[V3]] : $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V7:.*]] = apply %[[V4]](%[[V1]]) : $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: end_borrow %[[V4]] : $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: %[[V8:.*]] = tuple ()
// CHECK: destroy_addr %[[V1]] : $*ARCWeak
// CHECK: destroy_value %[[V3]] : $@callee_guaranteed (@in_guaranteed ARCWeak) -> ()
// CHECK: return %[[V8]] : $()

// ARCWeak is destroyed by the callee.
public func testARCWeak() {
  cfuncARCWeak({ARCWeak in})
}
