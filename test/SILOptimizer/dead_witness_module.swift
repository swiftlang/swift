// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Onone -parse-as-library  -module-name TestModule  %S/Inputs/TestModule.swift -emit-module-path %t/TestModule.swiftmodule 
// RUN: %target-swift-frontend -O %s -I %t -emit-sil | %FileCheck %s

// DeadFunctionElimination may not remove a method from a witness table which
// is imported from another module.

// FIXME: Ever since @usableFromInline began to be enforced, this test did not
// test anything useful, and now the witness table is never deserialized at all.

import TestModule

testit(MyStruct())

// CHECK-NOT: sil_witness_table MyStruct: Proto module TestModule
