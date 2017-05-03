struct S {
  let count = 1
}

public class CC {
  func foo(_ s : S) -> Int {
    if s.count > 0 {
      return 1
    } else {
      return 0
    }
  }
}

// RUN: %target-swift-ide-test -range -pos=7:8 -end-pos=7:19 -source-filename %s | %FileCheck %s -check-prefix=CHECK-BOOL
// CHECK-BOOL: <Type>Bool</Type>
