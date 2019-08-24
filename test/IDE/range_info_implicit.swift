public class CC {
  public init() {
  }
}

// RUN: %target-swift-ide-test -range -pos=3:1 -end-pos=3:4 -source-filename %s | %FileCheck %s -check-prefix=CHECK-INVALID
// CHECK-INVALID: <Kind>Invalid</Kind>
