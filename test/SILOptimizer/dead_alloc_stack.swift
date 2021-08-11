// RUN: %target-swift-frontend -O -emit-sil -parse-as-library %s | %FileCheck %s

protocol E {
  func f() -> Bool
}

protocol P {
  associatedtype A = Int
}

public struct X : P, E {
  func f() -> Bool { return true }
}

func g<T : P>(_ x : T) -> Bool {
  if let y = x as? E { return y.f() }
  return false
}

// Check that this function can be completely constant folded and no alloc_stack remains.

// CHECK-LABEL: sil @$s16dead_alloc_stack6testitySbAA1XVF
// CHECK:      bb0({{.*}}):
// CHECK-NEXT:   integer_literal
// CHECK-NEXT:   struct
// CHECK-NEXT:   return
// CHECK-NEXT: } // end sil function '$s16dead_alloc_stack6testitySbAA1XVF'
public func testit(_ x: X) -> Bool {
  return g(x)
}

