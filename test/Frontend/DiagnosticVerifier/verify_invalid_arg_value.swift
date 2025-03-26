// RUN: %target-swift-frontend -parse -verify=invalid1 -verify=invalid2 %s 2>&1 | %FileCheck %s

// CHECK-NOT: error:
// CHECK: error: unsupported argument 'invalid1' to option '-verify'
// CHECK-NEXT: error: unsupported argument 'invalid2' to option '-verify'
// CHECK-NOT: error:
