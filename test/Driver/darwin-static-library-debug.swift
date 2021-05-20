// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.15 -g -emit-library -static -o library.a %s 2>&1 | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-STATIC
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.15 -g -emit-library -o library.a %s 2>&1 | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-SHARED
// RUN: %swiftc_driver -driver-print-actions -target x86_64-apple-macosx10.15 -g -o a.out %s 2>&1 | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-EXEC

// CHECK: 0: input, "{{.*}}darwin-static-library-debug.swift", swift
// CHECK: 1: compile, {0}, object
// CHECK: 2: merge-module, {1}, swiftmodule
// CHECK-STATIC: 3: static-link, {1, 2}, image
// CHECK-STATIC-NOT: 4: generate-dSYM, {3}, dSYM
// CHECK-SHARED: 3: link, {1, 2}, image
// CHECK-SHARED: 4: generate-dSYM, {3}, dSYM
// CHECK-EXEC: 3: link, {1, 2}, image
// CHECK-EXEC: 4: generate-dSYM, {3}, dSYM

