// REQUIRES: asan_runtime
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-address-use-odr-indicator %s 2>&1 | %FileCheck %s

// CHECK: swift
// CHECK-DAG: -sanitize=address
// CHECK-DAG: -sanitize-address-use-odr-indicator

// Missing `-sanitize=address` causes warning to be emitted
// RUN: %swiftc_driver  -sanitize-address-use-odr-indicator \
// RUN:  %s -o %t 2>&1 | %FileCheck -check-prefix=CHECK-MISSING-ARG %s
// CHECK-MISSING-ARG: warning: option '-sanitize-address-use-odr-indicator' has no effect when 'address' sanitizer is disabled. Use -sanitize=address to enable the sanitizer
