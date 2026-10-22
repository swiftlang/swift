// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/single_yield_functions_other.swift \
// RUN:   -enable-experimental-feature SingleYieldFunctions \
// RUN:   -disable-experimental-parser-round-trip -experimental-skip-all-function-bodies
// RUN: %target-swift-ide-test -print-module -module-to-print=single_yield_functions_other \
// RUN:   -I %t -source-filename=%s \
// RUN:   -enable-experimental-feature SingleYieldFunctions | %FileCheck %s

// REQUIRES: swift_feature_SingleYieldFunctions

// Function bodies are skipped because SILGen doesn't support single-yield
// functions yet.

// CHECK: func borrowed(_ s: S) -> yield borrow Int
// CHECK: func mutable(_ s: inout S) -> yield inout Int
// CHECK: func owned(_ s: S) -> yield Box
