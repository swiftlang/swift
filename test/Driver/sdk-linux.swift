// RUN: %swiftc_driver -driver-print-jobs -g %s 2>&1 | FileCheck %s
// XFAIL: darwin

// CHECK-NOT: warning: no such SDK:
// CHECK: bin/swift
// CHECK: Driver/sdk-linux.swift
// CHECK: -sdk /{{ }}
// CHECK-NEXT: bin/swift
// CHECK: -sdk /{{ }}
