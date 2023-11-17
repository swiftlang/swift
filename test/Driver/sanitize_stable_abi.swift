// REQUIRES: asan_runtime
// RUN: %swiftc_driver -driver-print-jobs -sanitize=address -sanitize-stable-abi  %s 2>&1 | %FileCheck %s

// Link against stable ABI
// CHECK: libclang_rt.asan_abi{{.*}}.a
