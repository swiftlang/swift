let x = "an error type" + 3
let y = "not an error type"

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK-NOT: (1:5, 1:6)
// CHECK: (2:5, 2:6): String (explicit type: 0)
