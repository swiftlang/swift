// RUN: %swift-scan-test -action list_supported_arguments | %FileCheck %s

// Verify well-known frontend options are present.
// CHECK-DAG: target
// CHECK-DAG: sdk
// CHECK-DAG: I
// CHECK-DAG: module-name
// CHECK-DAG: emit-module
// CHECK-DAG: emit-object
// CHECK-DAG: parse
// CHECK-DAG: typecheck
// CHECK-DAG: O
// CHECK-DAG: g

// Verify that non-existent options are not present.
// CHECK-NOT: clearly-not-a-compiler-flag
