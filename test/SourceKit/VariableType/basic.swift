let x: Int = 3
let y = "abc"

var foo = ["abc" + "def"]

struct A {
  let x: String = ""
  let y = ""
}

class B {
  var x = 4.0
  var y = [A]()
  var z: [Int: Int] = [:]
  var w: (Int) -> Int { { $0 * 2 } }
}

func foo() {
  var local = 5
}

let `else` = 3

// RUN: %sourcekitd-test -req=collect-var-type %s -- %s | %FileCheck %s
// CHECK: (1:5, 1:6): Int (explicit type: 1)
// CHECK: (2:5, 2:6): String (explicit type: 0)
// CHECK: (4:5, 4:8): [String] (explicit type: 0)
// CHECK: (7:7, 7:8): String (explicit type: 1)
// CHECK: (8:7, 8:8): String (explicit type: 0)
// CHECK: (12:7, 12:8): Double (explicit type: 0)
// CHECK: (13:7, 13:8): [A] (explicit type: 0)
// CHECK: (14:7, 14:8): [Int : Int] (explicit type: 1)
// CHECK: (15:7, 15:8): (Int) -> Int (explicit type: 1)
// CHECK: (19:7, 19:12): Int (explicit type: 0)
// CHECK: (22:5, 22:11): Int (explicit type: 0)

// RUN: %sourcekitd-test -req=collect-var-type -req-opts=fully_qualified=true %s -- %s | %FileCheck %s --check-prefix CHECK-FULLY-QUALIFIED
// CHECK-FULLY-QUALIFIED: (1:5, 1:6): Swift.Int (explicit type: 1)
// CHECK-FULLY-QUALIFIED: (2:5, 2:6): Swift.String (explicit type: 0)
// CHECK-FULLY-QUALIFIED: (4:5, 4:8): [Swift.String] (explicit type: 0)
// CHECK-FULLY-QUALIFIED: (7:7, 7:8): Swift.String (explicit type: 1)
// CHECK-FULLY-QUALIFIED: (8:7, 8:8): Swift.String (explicit type: 0)
// CHECK-FULLY-QUALIFIED: (12:7, 12:8): Swift.Double (explicit type: 0)
// CHECK-FULLY-QUALIFIED: (13:7, 13:8): [basic.A] (explicit type: 0)
// CHECK-FULLY-QUALIFIED: (14:7, 14:8): [Swift.Int : Swift.Int] (explicit type: 1)
// CHECK-FULLY-QUALIFIED: (15:7, 15:8): (Swift.Int) -> Swift.Int (explicit type: 1)
// CHECK-FULLY-QUALIFIED: (19:7, 19:12): Swift.Int (explicit type: 0)
// CHECK-FULLY-QUALIFIED: (22:5, 22:11): Swift.Int (explicit type: 0)