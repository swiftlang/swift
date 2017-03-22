// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// Verify that reabstraction happens when forming container literals.
// <rdar://problem/16039286>

// CHECK-LABEL: sil hidden @_T025array_literal_abstraction0A9_of_funcsSayyycGyF
// CHECK:         pointer_to_address {{.*}} $*@callee_owned (@in ()) -> @out ()
func array_of_funcs() -> [(() -> ())] {
  return [{}, {}]
}

// CHECK-LABEL: sil hidden @_T025array_literal_abstraction13dict_of_funcss10DictionaryVySiyycGyF
// CHECK:         pointer_to_address {{.*}} $*(Int, @callee_owned (@in ()) -> @out ())
func dict_of_funcs() -> Dictionary<Int, () -> ()> {
  return [0: {}, 1: {}]
}

func vararg_funcs(_ fs: (() -> ())...) {}

// CHECK-LABEL: sil hidden @_T025array_literal_abstraction17call_vararg_funcsyyF
// CHECK:         pointer_to_address {{.*}} $*@callee_owned (@in ()) -> @out ()
func call_vararg_funcs() {
  vararg_funcs({}, {})
}
