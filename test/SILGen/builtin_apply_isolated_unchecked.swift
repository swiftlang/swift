// RUN: %target-swift-emit-silgen -parse-stdlib -target %target-swift-5.7-abi-triple %s | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Swift
import _Concurrency
import Distributed

actor Counter {
  var value = 0
}

// The closure is applied directly: no escaping conversion, no reabstraction
// thunk and no escape verification. `T` is generic, so the result is returned
// indirectly through the `$*T` slot passed as the first argument, and the
// typed error `E` is thrown indirectly through the `$*E` slot

// CHECK-LABEL: // applyActor<A, B, C>(_:_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ERROR:%.*]] : $*E, [[ACTOR:%.*]] : @guaranteed $A, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]], {{%.*}}, [[ACTOR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : AnyObject> (@sil_isolated @guaranteed τ_0_0) -> (@out τ_0_2, @error_indirect τ_0_1) for <A, E, T>, normal
// CHECK: throw_addr
// CHECK: } // end sil function
func applyActor<A: Actor, T, E: Error>(
  _ actor: A, _ fn: (isolated A) throws(E) -> T
) throws(E) -> T {
  try Builtin.applyActorIsolatedUnchecked(fn, actor)
}

// Distributed actors are class-bound too, so the same builtin applies

// CHECK-LABEL: // applyDistributedActor<A, B, C>(_:_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ERROR:%.*]] : $*E, [[ACTOR:%.*]] : @guaranteed $DA, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]], {{%.*}}, [[ACTOR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2 where τ_0_0 : AnyObject> (@sil_isolated @guaranteed τ_0_0) -> (@out τ_0_2, @error_indirect τ_0_1) for <DA, E, T>, normal
// CHECK: throw_addr
// CHECK: } // end sil function
func applyDistributedActor<DA: DistributedActor, T, E: Error>(
  _ actor: DA, _ fn: (isolated DA) throws(E) -> T
) throws(E) -> T {
  try Builtin.applyActorIsolatedUnchecked(fn, actor)
}

// Global actor isolation is generic: the main actor is not special

// CHECK-LABEL: // applyMain<A, B>(_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ERROR:%.*]] : $*E, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]], {{%.*}}) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <E, T>, normal
// CHECK: throw_addr
// CHECK: } // end sil function
func applyMain<T, E: Error>(_ fn: @MainActor () throws(E) -> T) throws(E) -> T {
  try Builtin.applyGlobalActorIsolatedUnchecked(fn)
}

@globalActor
actor CustomGlobalActor {
  static let shared = CustomGlobalActor()
}

// CHECK-LABEL: // applyCustomGlobal<A, B>(_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ERROR:%.*]] : $*E, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]], {{%.*}}) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1> () -> (@out τ_0_1, @error_indirect τ_0_0) for <E, T>, normal
// CHECK: throw_addr
// CHECK: } // end sil function
func applyCustomGlobal<T, E: Error>(
  _ fn: @CustomGlobalActor () throws(E) -> T
) throws(E) -> T {
  try Builtin.applyGlobalActorIsolatedUnchecked(fn)
}

// A non-throwing closure literal infers `E == Never`: the closure is emitted
// at its natural abstraction (direct Int result) and applied without an error
// path

// CHECK-LABEL: // applyNonThrowing(_:)
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Counter):
// CHECK-NOT: partial_apply
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: [[FN:%.*]] = thin_to_thick_function {{.*}} to $@noescape @callee_guaranteed (@sil_isolated @guaranteed Counter) -> Int
// CHECK: [[BORROW:%.*]] = begin_borrow [[FN]]
// CHECK: apply [[BORROW]]([[ACTOR]]) : $@noescape @callee_guaranteed (@sil_isolated @guaranteed Counter) -> Int
// CHECK-NOT: try_apply
// CHECK: } // end sil function
func applyNonThrowing(_ counter: Counter) -> Int {
  Builtin.applyActorIsolatedUnchecked({ counter in counter.value }, counter)
}

// Untyped throws is the `E == any Error` case

// CHECK-LABEL: // applyActorUntyped<A, B>(_:_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ACTOR:%.*]] : @guaranteed $A, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK: try_apply [[FN]]([[RESULT]], [[ACTOR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@sil_isolated @guaranteed τ_0_0) -> (@out τ_0_1, @error any Error) for <A, T>, normal
// CHECK: bb2({{%.*}} : @owned $any Error):
// CHECK: throw
// CHECK: } // end sil function
func applyActorUntyped<A: Actor, T>(_ actor: A, _ fn: (isolated A) throws -> T) throws -> T {
  try Builtin.applyActorIsolatedUnchecked(fn, actor)
}

struct NC: ~Copyable {
  var value: Int
}

// A noncopyable result is returned directly through the builtin

// CHECK-LABEL: // applyNoncopyable<A, B>(_:_:)
// CHECK: bb0([[ERROR:%.*]] : $*E, [[ACTOR:%.*]] : @guaranteed $A, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK: try_apply [[FN]]({{%.*}}, [[ACTOR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@sil_isolated @guaranteed τ_0_0) -> (@owned NC, @error_indirect τ_0_1) for <A, E>, normal
// CHECK: bb1([[NC:%.*]] : @owned $NC):
// CHECK: return [[NC]]
// CHECK: throw_addr
// CHECK: } // end sil function
func applyNoncopyable<A: Actor, E: Error>(
  _ actor: A, _ fn: (isolated A) throws(E) -> NC
) throws(E) -> NC {
  try Builtin.applyActorIsolatedUnchecked(fn, actor)
}
