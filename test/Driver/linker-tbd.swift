// REQUIRES: OS=macosx
// Ensure .tbd input files are forwarded to the linker.

// RUN: %empty-directory(%t)
// RUN: touch %t/foo.tbd
// RUN: %swiftc_driver_plain -driver-print-jobs -target x86_64-apple-macosx10.9 %S/../Inputs/empty.swift %t/foo.tbd | %FileCheck %s

// CHECK: bin/ld{{"? }}
// CHECK-SAME: foo.tbd
