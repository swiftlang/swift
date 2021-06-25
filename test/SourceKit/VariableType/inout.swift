func x(_ param: inout Int) -> Int {
  param = 4
}

let z = { (param: inout String) in }

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK: (1:10, 1:15): Int (explicit type: 1)
// CHECK: (5:5, 5:6): (inout String) -> () (explicit type: 0)
// CHECK: (5:12, 5:17): String (explicit type: 1)
