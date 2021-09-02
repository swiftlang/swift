// RUN: %target-swift-frontend -O -primary-file %s -emit-sil -enable-experimental-distributed | %FileCheck %s --dump-input=fail
// REQUIRES: concurrency

import _Distributed

class SomeClass {}

@available(SwiftStdlib 5.5, *)
actor SimpleActor {
  let someFieldInLocalActor: SomeClass
  init(field: SomeClass) {
    self.someFieldInLocalActor = field
  }
}


// ==== ------------------------------------------------------------------------
// ==== Check that a normal local only actor is left unchanged

// CHECK: // SimpleActor.deinit
// CHECK: sil hidden{{.*}} @$s42distributed_actor_remote_deinit_sil_normal11SimpleActorCfd : $@convention(method) (@guaranteed SimpleActor) -> @owned Builtin.NativeObject {
// CHECK: // %0 "self" // users: %6, %5, %2, %1
// CHECK: bb0(%0 : $SimpleActor):
// CHECK:  debug_value %0 : $SimpleActor, let, name "self", argno 1, implicit
// CHECK:  %2 = ref_element_addr %0 : $SimpleActor, #SimpleActor.someFieldInLocalActor
// CHECK:  %3 = load %2 : $*SomeClass // user: %4
// CHECK:  strong_release %3 : $SomeClass // id: %4
// CHECK:  %5 = builtin "destroyDefaultActor"(%0 : $SimpleActor) : $()
// CHECK:  %6 = unchecked_ref_cast %0 : $SimpleActor to $Builtin.NativeObject // user: %7
// CHECK:  return %6 : $Builtin.NativeObject // id: %7
// CHECK: } // end sil function '$s42distributed_actor_remote_deinit_sil_normal11SimpleActorCfd'

