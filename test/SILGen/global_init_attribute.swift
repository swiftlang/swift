// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-module -o %t %S/Inputs/def_global.swift
// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -parse-as-library -emit-silgen -I %t %s | %FileCheck %s
//
// Test that SILGen uses the "global_init" attribute for all global
// variable addressors.

import def_global

let InternalConst = 42
// CHECK-NOT: [global_init]
// CHECK: // global_init_attribute.InternalConst.unsafeMutableAddressor : Swift.Int
// CHECK-NEXT: sil hidden [global_init] @_T021global_init_attribute13InternalConstSifau

func foo() -> Int {
  return ExportedVar
}

func bar(i: Int) {
  ExportedVar = i
}

// CHECK-NOT: [global_init]
// CHECK: // def_global.ExportedVar.unsafeMutableAddressor : Swift.Int
// CHECK-NEXT: sil [global_init] @_T010def_global11ExportedVarSifau

var InternalFoo = foo()

// CHECK-NOT: [global_init]
// CHECK: // global_init_attribute.InternalFoo.unsafeMutableAddressor : Swift.Int
// CHECK-NEXT: sil hidden [global_init] @_T021global_init_attribute11InternalFooSifau
