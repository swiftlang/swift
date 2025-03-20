// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -o %t %S/Inputs/convention_c_function.swift
// RUN: llvm-bcanalyzer %t/convention_c_function.swiftmodule | %FileCheck -check-prefix=CHECK-BCANALYZER %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -I %t %s | %FileCheck %s

import convention_c_function

// CHECK-BCANALYZER-LABEL: (INDEX_BLOCK):
// CHECK-BCANALYZER: CLANG_TYPE_OFFSETS

// Test that the assertion in SILDeclRef doesn't fail.

// CHECK-LABEL: sil [ossa] @$s4main3baryyF : $@convention(thin) () -> () {
// CHECK: %[[V0:.*]] = function_ref @$s4main3baryyFyycfU_To : $@convention(c) () -> ()
// CHECK: %[[V1:.*]] = function_ref @$s21convention_c_function3foo2fnyyyXC_tF : $@convention(thin) (@convention(c) () -> ()) -> ()
// CHECK: apply %[[V1]](%[[V0]]) : $@convention(thin) (@convention(c) () -> ()) -> ()

public func bar() {
  foo(fn : {})
}

