// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %swift %S/Inputs/vtable_deserialization_input.swift -o %t/Swift.swiftmodule -emit-module -parse-as-library -parse-stdlib -module-link-name swift_stdlib_core -module-name Swift -sil-serialize-all
// RUN: %swift %s -emit-sil -O3 -I=%t -o - | FileCheck %s

import Swift

func WhatShouldIDoImBored<T : P>(t : T) {
  t.doSomething()
}

func MakeItNotAGlobal() -> Y {
  var x = Y()
  WhatShouldIDoImBored(x)
  return x
}


// Make sure all abstractions have been removed and everything inlined into top_level_method.
// CHECK-LABEL: sil private @top_level_code : $@thin () -> () {
// CHECK: bb0:
// CHECK-NEXT: alloc_ref
// CHECK-NEXT: function_ref unknown
// CHECK-NEXT: function_ref @unknown
// CHECK-NEXT: apply
// CHECK-NEXT: strong_release
// CHECK-NEXT: tuple
// CHECK-NEXT: return
// CHECK-NEXT: }
MakeItNotAGlobal()

// Make sure our vtable/witness tables are properly deserialized.
// CHECK: sil_vtable Y {
// CHECK: sil_witness_table public_external Y: P module Swift {
