func foo(_ a: Bool) -> Int{
  if a {
    return 1
  } else {
  }
  if a {
    return 0
  } else {
    return 1
  }
}

// RUN: %target-swift-ide-test -range -pos=2:1 -end-pos 5:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-ERR
// RUN: %target-swift-ide-test -range -pos=6:1 -end-pos 10:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INT

// CHECK-ERR: <Type><<error type>></Type>
// CHECK-ERR-NOT: <Exit>true</Exit>
// CHECK-INT: <Type>Int</Type>
// CHECK-INT: <Exit>true</Exit>
