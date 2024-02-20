// RUN: %target-swift-emit-silgen -verify %s | %FileCheck %s

func callee<T>(_: T) {}

func g() throws {}

func caller() {
  callee { try g() }
}

// CHECK-LABEL: sil private [ossa] @$s36literal_closure_reabstraction_throws6calleryyFyyKcfU_ : $@convention(thin) @substituted <τ_0_0> () -> (@out τ_0_0, @error any Error) for <()> {
// CHECK: bb0(%0 : $*()):
// CHECK:  debug_value undef : $any Error, var, name "$error", argno 1