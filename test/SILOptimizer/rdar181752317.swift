// RUN: %target-swift-emit-sil %s -O -disable-availability-checking -module-name=test | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-OPT
// RUN: %target-swift-emit-sil %s -Onone -disable-availability-checking -module-name=test | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-ONONE

// REQUIRES: swift_stdlib_no_asserts, optimized_stdlib

// MARK: rdar://181752317 (Large constant lookup tables cause stack overflows in debug)
//
// This specifically tests the case where the lookup table is in the main file
// and its initialiser is top-level code.
//
// CHECK-LABEL: sil {{.*}}@{{main|__main_argc_argv}} :
// CHECK-NOT:     alloc_stack
// CHECK:       } // end sil function '{{main|__main_argc_argv}}'
let largeLut: [_ of UInt16] = [
    0x0000, 0x0000, 0x07B5, 0x07B5, 0x0840, 0x0842, 0x0841, 0x083C, 0x0843, 0x083E, 0x083D, 0x0838, 0x083F, 0x083A, 0x0839, 0x0834,
    0x0000, 0x0000, 0x07B5, 0x07B5, 0x0840, 0x0842, 0x0841, 0x083C, 0x0843, 0x083E, 0x083D, 0x0838, 0x083F, 0x083A, 0x0839, 0x0834,
    0x0000, 0x0000, 0x07B5, 0x07B5, 0x0840, 0x0842, 0x0841, 0x083C, 0x0843, 0x083E, 0x083D, 0x0838, 0x083F, 0x083A, 0x0839, 0x0834,
    0x0000, 0x0000, 0x07B5, 0x07B5, 0x0840, 0x0842, 0x0841, 0x083C, 0x0843, 0x083E, 0x083D, 0x0838, 0x083F, 0x083A, 0x0839, 0xBEEF,
]
