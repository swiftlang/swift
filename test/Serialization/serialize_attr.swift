// RUN: rm -rf %t
// RUN: mkdir -p %t
// RUN: %target-swift-frontend -emit-module -parse-as-library -sil-serialize-all -o %t %s
// RUN: llvm-bcanalyzer %t/serialize_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -enable-sil-verify-all %t/serialize_attr.swiftmodule | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

// @_semantics
// -----------------------------------------------------------------------------

//CHECK-DAG: @_semantics("crazy") func foo()
@_semantics("crazy") func foo() -> Int  { return 5}

// @_specialize
// -----------------------------------------------------------------------------

// These lines should be contiguous.
// CHECK-DAG: @_specialize(exported: false, kind: full, where τ_0_0 == Int, τ_0_1 == Float)
// CHECK-DAG: func specializeThis<T, U>(_ t: T, u: U)
@_specialize(where T == Int, U == Float)
func specializeThis<T, U>(_ t: T, u: U) {}


