// RUN: %target-swift-frontend -O -module-name=test -emit-sil %s | %FileCheck %s

protocol P {}

public struct S {
  let p: P // force S to be an address-only type
  let i: Int
}

public final class X {
  let s: S

  init(s: S) {
    self.s = s
  }
}

@inline(never)
func createX(s: S) -> X {
  return X(s: s)
}

// Test that the load is a barrier for release hoisting and that the strong_release is not hoisted above the load.

// CHECK-LABEL: sil @$s4test6testit1sSiAA1SV_tF
// CHECK:         [[X:%[0-9]+]] = apply
// CHECK:         [[R:%[0-9]+]] = load
// CHECK:         strong_release [[X]]
// CHECK:         return [[R]]
// CHECK:       } // end sil function '$s4test6testit1sSiAA1SV_tF'
public func testit(s: S) -> Int {
  let x = createX(s: s)
  return x.s.i
}

