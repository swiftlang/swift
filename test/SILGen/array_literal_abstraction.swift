// RUN: %swift -emit-silgen %s | FileCheck %s

// Verify that reabstraction happens when forming container literals.
// <rdar://problem/16039286>

// CHECK-LABEL: sil @_TF25array_literal_abstraction14array_of_funcsFT_GSaFT_T__
// CHECK:         alloc_array $@callee_owned (@out (), @in ()) -> ()
func array_of_funcs() -> (() -> ())[] {
  return [{}, {}]
}

// CHECK-LABEL: sil @_TF25array_literal_abstraction13dict_of_funcsFT_GVSs10DictionarySiFT_T__
// CHECK:         alloc_array $(Int, @callee_owned (@out (), @in ()) -> ())
func dict_of_funcs() -> Dictionary<Int, () -> ()> {
  return [0: {}, 1: {}]
}

func vararg_funcs(fs: (() -> ())...) {}

// CHECK-LABEL: sil @_TF25array_literal_abstraction17call_vararg_funcsFT_T_
// CHECK:         alloc_array $@callee_owned (@out (), @in ()) -> ()
func call_vararg_funcs() {
  vararg_funcs({}, {})
}
