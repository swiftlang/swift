// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-as-library -sil-serialize-all -o %t %s
// RUN: llvm-bcanalyzer %t/global_init.swiftmodule | FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt %t/global_init.swiftmodule | FileCheck %s

// BCANALYZER-NOT: UnknownCode

// Swift globals are not currently serialized. However, addressor
// declarations are serialized when all these three flags are present:
// -emit-module -parse-as-library -sil-serialize-all
//
// The only way to inspect the serialized module is sil-opt. The swift
// driver will only output the SIL that it deserializes.

let MyConst = 42
var MyVar = 3

// CHECK: let MyConst: Int
// CHECK: var MyVar: Int

// CHECK-DAG: sil hidden [fragile] [global_init] @_TF11global_initau7MyConstSi : $@convention(thin) () -> Builtin.RawPointer
// CHECK-DAG: sil hidden [fragile] [global_init] @_TF11global_initau5MyVarSi : $@convention(thin) () -> Builtin.RawPointer

func getGlobals() -> Int {
  return MyVar + MyConst
}
