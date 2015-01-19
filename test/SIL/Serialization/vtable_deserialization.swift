// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend %S/Inputs/vtable_deserialization_input.swift -o %t/Swift.swiftmodule -emit-module -parse-as-library -parse-stdlib -module-link-name swiftCore -module-name Swift -sil-serialize-all
// RUN: %target-swift-frontend %s -emit-sil -O -I %t -o - | FileCheck %s

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
// CHECK-LABEL: sil @main
// CHECK-NEXT: bb0({{.*}}):
// CHECK-NEXT: function_ref unknown
// CHECK-NEXT: function_ref @unknown
// CHECK-NEXT: apply
// CHECK-NEXT: integer_literal
// CHECK-NEXT: return
// CHECK-NEXT: }
MakeItNotAGlobal()

// Make sure our vtable/witness tables are properly deserialized.
// CHECK: sil_vtable Y {
// CHECK: sil_witness_table public_external [fragile] Y: P module Swift {
