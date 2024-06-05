// RUN: %target-swiftxx-frontend -I %S/Inputs -emit-silgen %s | %FileCheck %s

import Closure

// CHECK: sil [ossa] @$s4main20testClosureToFuncPtryyF : $@convention(thin) () -> () {
// CHECK: %[[V0:.*]] = function_ref @$s4main20testClosureToFuncPtryyFySo10NonTrivialVcfU_To : $@convention(c) (@in NonTrivial) -> ()
// CHECK: %[[V1:.*]] = enum $Optional<@convention(c) (@in NonTrivial) -> ()>, #Optional.some!enumelt, %[[V0]] : $@convention(c) (@in NonTrivial) -> ()
// CHECK: %[[V2:.*]] = function_ref @{{.*}}cfunc2{{.*}}NonTrivial{{.*}} : $@convention(c) (Optional<@convention(c) (@in NonTrivial) -> ()>) -> ()
// CHECK: %[[V3:.*]] = apply %[[V2]](%[[V1]]) : $@convention(c) (Optional<@convention(c) (@in NonTrivial) -> ()>) -> ()

// CHECK: sil private [ossa] @$s4main20testClosureToFuncPtryyFySo10NonTrivialVcfU_ : $@convention(thin) (@in_guaranteed NonTrivial) -> () {

// CHECK: sil private [thunk] [ossa] @$s4main20testClosureToFuncPtryyFySo10NonTrivialVcfU_To : $@convention(c) (@in NonTrivial) -> () {
// CHECK: bb0(%[[V0:.*]] : $*NonTrivial):
// CHECK: %[[V1:.*]] = function_ref @$s4main20testClosureToFuncPtryyFySo10NonTrivialVcfU_ : $@convention(thin) (@in_guaranteed NonTrivial) -> ()
// CHECK: %[[V2:.*]] = apply %[[V1]](%[[V0]]) : $@convention(thin) (@in_guaranteed NonTrivial) -> ()
// CHECK: destroy_addr %[[V0]] : $*NonTrivial

public func testClosureToFuncPtr() {
 cfunc2({N in})
}

// CHECK: sil private [thunk] [ossa] @$s4main36testClosureToFuncPtrReturnNonTrivialyyFSo0hI0VycfU_To : $@convention(c) () -> @out NonTrivial {
// CHECK: bb0(%[[V0:.*]] : $*NonTrivial):
// CHECK: %[[V1:.*]] = function_ref @$s4main36testClosureToFuncPtrReturnNonTrivialyyFSo0hI0VycfU_ : $@convention(thin) () -> @out NonTrivial
// CHECK: %[[V2:.*]] = apply %[[V1]](%[[V0]]) : $@convention(thin) () -> @out NonTrivial
// CHECK: return %[[V2]] : $()

public func testClosureToFuncPtrReturnNonTrivial() {
  cfuncReturnNonTrivial2({() -> NonTrivial in return NonTrivial()});
}
