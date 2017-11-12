// This test will crash if we end up doing unnecessary typechecking from the secondary file.

// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -emit-sil -primary-file %s %S/Inputs/forbid_typecheck_2.swift -debug-forbid-typecheck-prefix NOTYPECHECK | %FileCheck %s

// CHECK: check_unnecessary_typecheck.globalPrim
let globalPrim = globalSec

// CHECK: check_unnecessary_typecheck.primFn
func primFn() {
  secFn()
  let _ = ClsSec().member
}
