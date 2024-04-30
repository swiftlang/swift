// RUN: %target-swiftxx-frontend -I %S/Inputs -emit-silgen %s | %FileCheck %s

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
