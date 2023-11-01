// RUN: %target-swift-emit-silgen %s -enable-experimental-feature TypedThrows -enable-experimental-feature FullTypedThrows | %FileCheck %s

public func genericThrow<E>(e: E) throws(E) -> () {
  throw e
}

// CHECK-LABEL: sil [ossa] @$s20typed_throws_generic0C5Throw1eyx_txYKs5ErrorRzlF : $@convention(thin) <E where E : Error> (@in_guaranteed E) -> @error_indirect E {

// CHECK: bb0(%0 : $*E, %1 : $*E):
// CHECK: [[TMP:%.*]] = alloc_stack $E
// CHECK: copy_addr %1 to [init] [[TMP]] : $*E
// CHECK: copy_addr [take] [[TMP]] to [init] %0 : $*E
// CHECK: dealloc_stack [[TMP]] : $*E
// CHECK: throw_addr


public func genericTryApply<E>(fn: () throws(E) -> ()) throws(E) -> () {
  try fn()
}

// CHECK-LABEL: sil [ossa] @$s20typed_throws_generic0C8TryApply2fnyyyxYKXE_txYKs5ErrorRzlF : $@convention(thin) <E where E : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>) -> @error_indirect E {

// CHECK: bb0(%0 : $*E, %1 : @guaranteed $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>):
// CHECK: [[FN:%.*]] = copy_value %1 : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: [[ERROR:%.*]] = alloc_stack $E
// CHECK: [[FN_BORROW:%.*]] = begin_borrow [[FN]] : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>
// CHECK: try_apply [[FN_BORROW]]([[ERROR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>, normal bb1, error bb2

// CHECK: bb1(%7 : $()):
// CHECK: end_borrow [[FN_BORROW]]
// CHECK: dealloc_stack [[ERROR]]
// CHECK: destroy_value [[FN]]
// CHECK: return

// CHECK: bb2:
// CHECK: copy_addr [take] [[ERROR]] to [init] %0 : $*E
// CHECK: end_borrow [[FN_BORROW]]
// CHECK: dealloc_stack [[ERROR]]
// CHECK: destroy_value [[FN]]
// CHECK: throw_addr
