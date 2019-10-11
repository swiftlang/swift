// RUN: %target-swift-frontend -O -parse-as-library -module-name=test -emit-sil %s | %FileCheck %s

final class X {}

public struct S {
  var x: X
  var i: Int
}

var gg = X()

@inline(never)
func takex(_ x: X) {
  gg = x
}

// Test if escape analysis is able to handle inout parameters, containing
// references, correctly.

// CHECK-LABEL: sil @$s4test6testityyAA1SVz_ADtF
// CHECK-NOT: store
// CHECK:     apply
// CHECK:     store
// CHECK-NOT: store
// CHECK:    } // end sil function '$s4test6testityyAA1SVz_ADtF'
public func testit(_ s: inout S, _ s1: S) {
   s = s1      // this store should be eliminated, even if s.x escapes.
   takex(s.x)
   s = s1
}
