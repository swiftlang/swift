func foo() { return 1 }

// RUN: %sourcekitd-test -req=collect-type %s -- %s | %FileCheck %s
// CHECK: (20, 21): Int
