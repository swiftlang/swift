// RUN: %target-swift-frontend -O -module-name=test -emit-sil -primary-file %s | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts


// Test that even with a generic array the iteration is done efficiently.

// CHECK-LABEL: sil @$s4test0A15ContiguousArrayySis0bC0VyxG_SixXEtlF : $@convention(thin) <Element> (@guaranteed ContiguousArray<Element>, @guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> (@in_guaranteed τ_0_0) -> Int for <Element>) -> Int {
// CHECK-NOT:     function_ref
// CHECK-NOT:     method
// CHECK:       } // end sil function '$s4test0A15ContiguousArrayySis0bC0VyxG_SixXEtlF'
public func testContiguousArray<Element>(_ a: ContiguousArray<Element>, _ c: (Element) -> Int) -> Int {
  var s = 0
  for x in a {
    s += c(x)
  }
  return s
}

// CHECK-LABEL: sil @$s4test0A4SwapyySaySSGz_S2itF :
// CHECK-NOT:     retain
// CHECK-NOT:     release
// CHECK:       } // end sil function '$s4test0A4SwapyySaySSGz_S2itF'
public func testSwap(_ a: inout [String], _ i: Int, _ j: Int) {
  a.swapAt(i, j)
}

// CHECK-LABEL: sil @$s4test0A14SwapViaWitnessyySaySSGz_S2itF :
// CHECK-NOT:     retain
// CHECK-NOT:     release
// CHECK:       } // end sil function '$s4test0A14SwapViaWitnessyySaySSGz_S2itF'
func swapit<C: RangeReplaceableCollection & MutableCollection>(_ c: inout C, _ i: C.Index, _ j: C.Index) {
  c.swapAt(i, j)
}

public func testSwapViaWitness(_ a: inout [String], _ i: Int, _ j: Int) {
  swapit(&a, i, j)
}

