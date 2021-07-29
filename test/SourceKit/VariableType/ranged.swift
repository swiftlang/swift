let x = 1
let y = "abc"
let z: String = "def"
var w = 4

// RUN: %sourcekitd-test -req=collect-var-type -pos=2:1 -end-pos=4:1 %s -- %s | %FileCheck %s
// CHECK-NOT: (1:5, 1:6)
// CHECK: (2:5, 2:6): String (explicit type: 0)
// CHECK: (3:5, 3:6): String (explicit type: 1)
// CHECK-NOT: (4:5, 4:6)
