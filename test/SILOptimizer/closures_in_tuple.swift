// RUN: %target-swift-frontend -primary-file %s -O -module-name=test -emit-sil | %FileCheck %s

func makeClosures() -> (add: (Double) -> (), sum: () -> Double) {
  var sum: Double = 0.0
  return (
    add: { (x: Double) -> () in sum += x },
    sum: { sum }
  )
}

// Check that closures, which are returned in a tuple, are inlined and the context box is eliminated.

// CHECK-LABEL: sil hidden @$s4test0A8ClosuresySdSiF :
// CHECK-NOT:     alloc_box
// CHECK-NOT:     partial_apply
// CHECK-NOT:     apply
// CHECK:         builtin "fadd_FPIEEE64"
// CHECK-NOT:     apply
// CHECK:       } // end sil function '$s4test0A8ClosuresySdSiF'
func testClosures(_ n: Int) -> Double {
  let c = makeClosures()
  for _ in 0 ..< n { c.add(0.01) }
  return c.sum()
}
