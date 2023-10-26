// RUN: %target-swift-emit-silgen %s -enable-experimental-feature TypedThrows | %FileCheck %s

public func genericThrows<E>(_: () throws(E) -> ()) throws(E) -> () {}

// CHECK-LABEL: sil [ossa] @$s20typed_throws_generic0C6ThrowsyyyyKXEKs5ErrorRzlF : $@convention(thin) <E where E : Error> (@guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>) -> @error_indirect E {
// CHECK: bb0(%0 : $*E, %1 : @guaranteed $@noescape @callee_guaranteed @substituted <τ_0_0> () -> @error_indirect τ_0_0 for <E>):