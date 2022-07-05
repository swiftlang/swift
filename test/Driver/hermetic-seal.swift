// RUN: %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s \
// RUN:   -experimental-hermetic-seal-at-link -lto=llvm-full 2>&1 | %FileCheck %s

// CHECK: swift
// CHECK: -enable-llvm-vfe
// CHECK: -enable-llvm-wme
// CHECK: -conditional-runtime-records
// CHECK: -internalize-at-link
// CHECK: -lto=llvm-full

// RUN: not %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s \
// RUN:   -experimental-hermetic-seal-at-link -enable-library-evolution 2>&1 | %FileCheck %s --check-prefix CHECK-LE

// CHECK-LE: error: Cannot use -experimental-hermetic-seal-at-link with -enable-library-evolution

// RUN: not %swiftc_driver -driver-print-jobs -target x86_64-apple-macosx10.9 %s \
// RUN:   -experimental-hermetic-seal-at-link 2>&1 | %FileCheck %s --check-prefix CHECK-NOLTO

// CHECK-NOLTO: error: -experimental-hermetic-seal-at-link requires -lto=llvm-full or -lto=llvm-thin
