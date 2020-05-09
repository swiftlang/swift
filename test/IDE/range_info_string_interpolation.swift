func test(x: Int, y: Int) {
  let z = "x = \(x), y = \(y)"
  print(z)
}

// RUN: %target-swift-ide-test -range -pos=2:1 -end-pos 4:1 -source-filename %s | %FileCheck %s -check-prefix=CHECK-PARAMS

// CHECK-PARAMS: <Referenced>x</Referenced><Type>Int</Type>
// CHECK-PARAMS: <Referenced>y</Referenced><Type>Int</Type>
