// REQUIRES: OS=macosx

// RUN: %swiftc_driver -driver-print-jobs %s -target x86_64-apple-macosx10.9 | %FileCheck %s --check-prefix=CHECK-DEFAULT-LD
// CHECK-DEFAULT-LD: /usr/bin/ld

// RUN: %swiftc_driver -driver-print-jobs %s -target x86_64-apple-macosx10.9 -use-ld=some-ld | %FileCheck %s --check-prefix=CHECK-USE-LD-SOME-LD
// CHECK-USE-LD-SOME-LD: some-ld

// RUN: %swiftc_driver -driver-print-jobs %s -target x86_64-apple-macosx10.9 -use-ld=lld -tools-directory %S/Inputs/fake-toolchain | %FileCheck %s --check-prefix=CHECK-USE-LD-TOOLS-DIRECTORY
// CHECK-USE-LD-TOOLS-DIRECTORY: lld

// RUN: %swiftc_driver -driver-print-jobs %s -target x86_64-apple-macosx10.9 -use-ld=/full/path/to/ld | %FileCheck %s --check-prefix=CHECK-USE-LD-FULL-PATH
// CHECK-USE-LD-FULL-PATH: /full/path/to/ld
