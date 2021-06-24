func x(_ param: Int) -> Int {
  param
}

let y: (String) -> Void = { param in }

let z = { (param: String) in
  param.count
}

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK: (1:10, 1:15): Int (explicit type: 1)
// CHECK: (5:5, 5:6): (String) -> Void (explicit type: 1)
// CHECK: (5:29, 5:34): String (explicit type: 0)
// CHECK: (7:5, 7:6): (String) -> Int (explicit type: 0)
// CHECK: (7:12, 7:17): String (explicit type: 1)
