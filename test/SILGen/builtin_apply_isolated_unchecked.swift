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
// indirectly through the `$*T` slot passed as the first argument

// CHECK-LABEL: // applyActor<A, B>(_:_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ACTOR:%.*]] : @guaranteed $A, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]], [[ACTOR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@sil_isolated @guaranteed τ_0_0) -> (@out τ_0_1, @error any Error) for <A, T>, normal
// CHECK: } // end sil function
func applyActor<A: Actor, T>(_ actor: A, _ fn: (isolated A) throws -> T) rethrows -> T {
  try Builtin.applyActorIsolatedUnchecked(fn, actor)
}

// Distributed actors are class-bound too, so the same builtin applies

// CHECK-LABEL: // applyDistributedActor<A, B>(_:_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[ACTOR:%.*]] : @guaranteed $DA, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]], [[ACTOR]]) : $@noescape @callee_guaranteed @substituted <τ_0_0, τ_0_1 where τ_0_0 : AnyObject> (@sil_isolated @guaranteed τ_0_0) -> (@out τ_0_1, @error any Error) for <DA, T>, normal
// CHECK: } // end sil function
func applyDistributedActor<DA: DistributedActor, T>(
  _ actor: DA, _ fn: (isolated DA) throws -> T
) rethrows -> T {
  try Builtin.applyActorIsolatedUnchecked(fn, actor)
}

// Global actor isolation is generic: the main actor is not special

// CHECK-LABEL: // applyMain<A>(_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]]) : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <T>, normal
// CHECK: } // end sil function
func applyMain<T>(_ fn: @MainActor () throws -> T) rethrows -> T {
  try Builtin.applyGlobalActorIsolatedUnchecked(fn)
}

@globalActor
actor CustomGlobalActor {
  static let shared = CustomGlobalActor()
}

// CHECK-LABEL: // applyCustomGlobal<A>(_:)
// CHECK: bb0([[RESULT:%.*]] : $*T, [[FN:%.*]] : @guaranteed $@noescape @callee_guaranteed
// CHECK-NOT: partial_apply
// CHECK-NOT: convert_escape_to_noescape
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: try_apply [[FN]]([[RESULT]]) : $@noescape @callee_guaranteed @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <T>, normal
// CHECK: } // end sil function
func applyCustomGlobal<T>(_ fn: @CustomGlobalActor () throws -> T) rethrows -> T {
  try Builtin.applyGlobalActorIsolatedUnchecked(fn)
}

// A non-throwing closure literal through the rethrowing builtin: the closure
// is emitted at its natural abstraction (direct Int result) and the error
// path is unreachable

// CHECK-LABEL: // applyNonThrowing(_:)
// CHECK: bb0([[GLOBAL_ACTOR:%.*]] : @guaranteed $Counter):
// CHECK-NOT: partial_apply
// CHECK-NOT: destroy_not_escaped_closure
// CHECK: [[FN:%.*]] = thin_to_thick_function {{.*}} to $@noescape @callee_guaranteed (@sil_isolated @guaranteed Counter) -> (Int, @error any Error)
// CHECK: [[BORROW:%.*]] = begin_borrow [[FN]]
// CHECK: try_apply [[BORROW]]([[GLOBAL_ACTOR]])
// CHECK: bb2({{.*}} : @owned $any Error):
// CHECK-NEXT: destroy_value [dead_end]
// CHECK-NEXT: unreachable
// CHECK: } // end sil function
func applyNonThrowing(_ counter: Counter) -> Int {
  Builtin.applyActorIsolatedUnchecked({ counter in counter.value }, counter)
}
