// RUN: %swiftc_driver -driver-print-jobs %S/../Inputs/empty.swift -lto=llvm-thin -target x86_64-apple-macosx10.9 | %FileCheck %s --check-prefix=CHECK-SIMPLE-THIN

// CHECK-SIMPLE-THIN: swift
// CHECK-SIMPLE-THIN-DAG: -emit-bc
// CHECK-SIMPLE-THIN-DAG: -lto=llvm-thin
// CHECK-SIMPLE-THIN-DAG: -o [[BITCODEFILE:.*\.bc]]


// RUN: %swiftc_driver -driver-print-jobs %S/../Inputs/empty.swift -lto=llvm-full -target x86_64-apple-macosx10.9 | %FileCheck %s --check-prefix=CHECK-SIMPLE-FULL

// CHECK-SIMPLE-FULL: swift
// CHECK-SIMPLE-FULL-DAG: -emit-bc
// CHECK-SIMPLE-FULL-DAG: -lto=llvm-full
// CHECK-SIMPLE-FULL-DAG: -o [[BITCODEFILE:.*\.bc]]
