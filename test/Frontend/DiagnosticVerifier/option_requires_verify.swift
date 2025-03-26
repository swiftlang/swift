/// Test that we warn about these options being ignored without `-verify`.

// RUN: %target-swift-frontend -parse -verify-additional-file %s -verify-additional-prefix p1- -verify-additional-prefix p2- -verify-ignore-unknown %s 2>&1 | %FileCheck %s

// CHECK-NOT: warning:
// CHECK: warning: ignoring -verify-ignore-unknown (requires -verify)
// CHECK-NEXT: warning: ignoring -verify-additional-file (requires -verify)
// CHECK-NEXT: warning: ignoring -verify-additional-prefix (requires -verify)
// CHECK-NOT: warning:
