// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend %S/Inputs/vtable_deserialization_input.swift -o %t/Swift.swiftmodule -emit-module -parse-as-library -parse-stdlib -module-link-name swiftCore -module-name Swift -sil-serialize-all
// RUN: %target-swift-frontend %s -emit-sil -O -I %t -o - | %FileCheck %s

import Swift

func WhatShouldIDoImBored<T : P>(_ t : T) {
  t.doSomething()
}

@inline(__always)
func MakeItNotAGlobal() -> Y {
  let x = Y()
  WhatShouldIDoImBored(x)
  return x
}


// Make sure all abstractions have been removed and everything inlined into top_level_method.
// CHECK-LABEL: sil @main
// CHECK: bb0({{.*}}):
// CHECK: [[UNKNOWN:%.*]] = function_ref @unknown
// CHECK: apply [[UNKNOWN]]
// CHECK: integer_literal
// CHECK: return
MakeItNotAGlobal()

// Make sure our vtable/witness tables are properly deserialized.
// CHECK: sil_vtable Y {
// CHECK: sil_witness_table public_external [serialized] Y: P module Swift {
