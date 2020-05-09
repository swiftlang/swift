
// RUN: %target-swift-emit-silgen -module-name array_literal_abstraction %s | %FileCheck %s

// Verify that reabstraction happens when forming container literals.
// <rdar://problem/16039286>

// CHECK-LABEL: sil hidden [ossa] @$s25array_literal_abstraction0A9_of_funcsSayyycGyF
// CHECK:         pointer_to_address {{.*}} $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>
func array_of_funcs() -> [(() -> ())] {
  return [{}, {}]
}

// CHECK-LABEL: sil hidden [ossa] @$s25array_literal_abstraction13dict_of_funcsSDySiyycGyF
// CHECK:         pointer_to_address {{.*}} $*(Int, @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>)
func dict_of_funcs() -> Dictionary<Int, () -> ()> {
  return [0: {}, 1: {}]
}

func vararg_funcs(_ fs: (() -> ())...) {}

// CHECK-LABEL: sil hidden [ossa] @$s25array_literal_abstraction17call_vararg_funcsyyF
// CHECK:         pointer_to_address {{.*}} $*@callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>
func call_vararg_funcs() {
  vararg_funcs({}, {})
}
