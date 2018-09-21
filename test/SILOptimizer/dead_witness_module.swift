// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -Onone -parse-stdlib -parse-as-library  -module-name TestModule  %S/Inputs/TestModule.swift -emit-module-path %t/TestModule.swiftmodule 
// RUN: %target-swift-frontend -O %s -I %t -emit-sil | %FileCheck %s

// DeadFunctionElimination may not remove a method from a witness table which
// is imported from another module.

import TestModule

testit(MyStruct())

// CHECK: sil_witness_table public_external MyStruct: Proto module TestModule
// CHECK-NEXT: method #Proto.confx!1: {{.*}} : @$s{{.*}}confx{{.*}}FTW
