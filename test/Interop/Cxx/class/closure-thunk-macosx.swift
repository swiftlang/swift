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

// CHECK-LABEL: sil private [thunk] [ossa] @$s4main22testConstRefNonTrivialyyFySo0eF0VcfU_To : $@convention(c) (@in_guaranteed NonTrivial) -> () {
// CHECK: bb0(%[[V0:.*]] : $*NonTrivial):
// CHECK: %[[V1:.*]] = alloc_stack $NonTrivial
// CHECK: copy_addr %[[V0]] to [init] %[[V1]] : $*NonTrivial
// CHECK: %[[V3:.*]] = function_ref @$s4main22testConstRefNonTrivialyyFySo0eF0VcfU_ : $@convention(thin) (@in_guaranteed NonTrivial) -> ()
// CHECK: %[[V4:.*]] = apply %[[V3]](%[[V1]]) : $@convention(thin) (@in_guaranteed NonTrivial) -> ()
// CHECK: destroy_addr %[[V1]] : $*NonTrivial
// CHECK: dealloc_stack %[[V1]] : $*NonTrivial
// CHECK: return %[[V4]] : $()

public func testConstRefNonTrivial() {
  cfuncConstRefNonTrivial({S in });
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s4main23testConstRefNonTrivial2yyFySo0E7TrivialVcfU_To : $@convention(c) (@in_guaranteed NonTrivial) -> () {
// CHECK: bb0(%[[V0:.*]] : $*NonTrivial):
// CHECK: %[[V1:.*]] = alloc_stack $NonTrivial
// CHECK: copy_addr %[[V0]] to [init] %[[V1]] : $*NonTrivial
// CHECK: %[[V3:.*]] = function_ref @$s4main23testConstRefNonTrivial2yyFySo0E7TrivialVcfU_ : $@convention(thin) (@in_guaranteed NonTrivial) -> ()
// CHECK: %[[V4:.*]] = apply %[[V3]](%[[V1]]) : $@convention(thin) (@in_guaranteed NonTrivial) -> ()
// CHECK: destroy_addr %[[V1]] : $*NonTrivial
// CHECK: dealloc_stack %[[V1]] : $*NonTrivial
// CHECK: return %[[V4]] : $()
public func testConstRefNonTrivial2() {
  cfuncConstRefNonTrivial(({S in }));
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s4main19testConstRefTrivialyyFySo0E0VcfU_To : $@convention(c) (@in_guaranteed Trivial) -> () {
// CHECK: bb0(%[[V0:.*]] : $*Trivial):
// CHECK: %[[V1:.*]] = load [trivial] %[[V0]] : $*Trivial
// CHECK: %[[V2:.*]] = function_ref @$s4main19testConstRefTrivialyyFySo0E0VcfU_ : $@convention(thin) (Trivial) -> ()
// CHECK: %[[V3:.*]] = apply %[[V2]](%[[V1]]) : $@convention(thin) (Trivial) -> ()
// CHECK: return %[[V3]] : $()

public func testConstRefTrivial() {
  cfuncConstRefTrivial({S in });
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s4main18testConstRefStrongyyFySo9ARCStrongVcfU_To : $@convention(c) (@in_guaranteed ARCStrong) -> () {
// CHECK: bb0(%[[V0:.*]] : $*ARCStrong):
// CHECK: %[[V1:.*]] = alloc_stack $ARCStrong
// CHECK: copy_addr %[[V0]] to [init] %[[V1]] : $*ARCStrong
// CHECK: %[[V3:.*]] = load [copy] %[[V1]] : $*ARCStrong
// CHECK: %[[V4:.*]] = begin_borrow %[[V3]] : $ARCStrong
// CHECK: %[[V5:.*]] = function_ref @$s4main18testConstRefStrongyyFySo9ARCStrongVcfU_ : $@convention(thin) (@guaranteed ARCStrong) -> ()
// CHECK: %[[V6:.*]] = apply %[[V5]](%[[V4]]) : $@convention(thin) (@guaranteed ARCStrong) -> ()
// CHECK: end_borrow %[[V4]] : $ARCStrong
// CHECK: destroy_value %[[V3]] : $ARCStrong
// CHECK: destroy_addr %[[V1]] : $*ARCStrong
// CHECK: dealloc_stack %[[V1]] : $*ARCStrong
// CHECK: return %[[V6]] : $()

public func testConstRefStrong() {
  cfuncConstRefStrong({S in });
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSo10NonTrivialVIegn_ABIeyBn_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> (), @in_guaranteed NonTrivial) -> () {
// CHECK: bb0(%[[V0:.*]] : $*@block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> (), %[[V1:.*]] : $*NonTrivial):
// CHECK: %[[V2:.*]] = project_block_storage %[[V0]] : $*@block_storage @callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK: %[[V3:.*]] = load [copy] %[[V2]] : $*@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK: %[[V4:.*]] = begin_borrow %[[V3]] : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK: apply %[[V4]](%[[V1]]) : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK: end_borrow %[[V4]] : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()
// CHECK: destroy_value %[[V3]] : $@callee_guaranteed (@in_guaranteed NonTrivial) -> ()

public func testBlockConstRefNonTrivial() {
  blockConstRefNonTrivial({S in });
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSo7TrivialVIegy_ABIeyBn_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (Trivial) -> (), @in_guaranteed Trivial) -> () {
// CHECK: bb0(%[[V0:.*]] : $*@block_storage @callee_guaranteed (Trivial) -> (), %[[V1:.*]] : $*Trivial):
// CHECK: %[[V2:.*]] = project_block_storage %[[V0]] : $*@block_storage @callee_guaranteed (Trivial) -> ()
// CHECK: %[[V3:.*]] = load [copy] %[[V2]] : $*@callee_guaranteed (Trivial) -> ()
// CHECK: %[[V4:.*]] = load [trivial] %[[V1]] : $*Trivial
// CHECK: %[[V5:.*]] = begin_borrow %[[V3]] : $@callee_guaranteed (Trivial) -> ()
// CHECK: apply %[[V5]](%[[V4]]) : $@callee_guaranteed (Trivial) -> ()
// CHECK: end_borrow %[[V5]] : $@callee_guaranteed (Trivial) -> ()
// CHECK: destroy_value %[[V3]] : $@callee_guaranteed (Trivial) -> ()

public func testBlockConstRefTrivial() {
  blockConstRefTrivial({S in });
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSo9ARCStrongVIegg_ABIeyBn_TR : $@convention(c) (@inout_aliasable @block_storage @callee_guaranteed (@guaranteed ARCStrong) -> (), @in_guaranteed ARCStrong) -> () {
// CHECK: bb0(%[[V0:.*]] : $*@block_storage @callee_guaranteed (@guaranteed ARCStrong) -> (), %[[V1:.*]] : $*ARCStrong):
// CHECK: %[[V2:.*]] = project_block_storage %[[V0]] : $*@block_storage @callee_guaranteed (@guaranteed ARCStrong) -> ()
// CHECK: %[[V3:.*]] = load [copy] %[[V2]] : $*@callee_guaranteed (@guaranteed ARCStrong) -> ()
// CHECK: %[[V4:.*]] = load_borrow %[[V1]] : $*ARCStrong
// CHECK: %[[V5:.*]] = begin_borrow %[[V3]] : $@callee_guaranteed (@guaranteed ARCStrong) -> ()
// CHECK: apply %[[V5]](%[[V4]]) : $@callee_guaranteed (@guaranteed ARCStrong) -> ()
// CHECK: end_borrow %[[V5]] : $@callee_guaranteed (@guaranteed ARCStrong) -> ()
// CHECK: end_borrow %[[V4]] : $ARCStrong
// CHECK: destroy_value %[[V3]] : $@callee_guaranteed (@guaranteed ARCStrong) -> ()

public func testBlockConstRefStrong() {
  blockConstRefStrong({S in });
}
