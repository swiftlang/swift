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

func foo1(_ a: Bool) {
  if a {}
  if a {}
  else {}
  if a {
    return
  } else {
    return
  }
}

// RUN: %target-swift-ide-test -range -pos=2:1 -end-pos 5:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-ERR
// RUN: %target-swift-ide-test -range -pos=6:1 -end-pos 10:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INT
// RUN: %target-swift-ide-test -range -pos=14:1 -end-pos 14:10 -source-filename %s | %FileCheck %s -check-prefix=CHECK-VOID-NO-RETURN
// RUN: %target-swift-ide-test -range -pos=15:1 -end-pos 16:10 -source-filename %s | %FileCheck %s -check-prefix=CHECK-VOID-NO-RETURN
// RUN: %target-swift-ide-test -range -pos=17:1 -end-pos 21:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-VOID-RETURN

// CHECK-ERR: <Type><<error type>></Type>
// CHECK-ERR-NOT: <Exit>true</Exit>

// CHECK-INT: <Type>Int</Type>
// CHECK-INT: <Exit>true</Exit>

// CHECK-VOID-NO-RETURN: <Type>Void</Type>
// CHECK-VOID-NO-RETURN-NOT: <Exit>true</Exit>

// CHECK-VOID-RETURN: <Type>Void</Type>
// CHECK-VOID-RETURN: <Exit>true</Exit>
