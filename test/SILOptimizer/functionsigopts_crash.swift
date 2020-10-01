// RUN: %target-swift-frontend -module-name main -O -emit-sil -primary-file %s | %FileCheck %s

protocol P {
  func foo()
}

public struct Inner {
  var x: Int
  var y: Int
}

public struct S : P {
  var i: Inner

  func foo() {
    print(i.x)
  }
}

// Check that FSO does not crash due to a missing decl on the function argument.

// Following specializations should be done:
//   * FSO: existential to protocol constrained generic
//   * generic specialization <S>
//   * FSO: argument explosion 

// CHECK-LABEL: sil shared [noinline] @$s4main6testityyAA1P_pFTf4e_nAA1SV_Tg5Tf4x_n : $@convention(thin) (Int) -> () { 
// CHECK-NEXT: // %0 "p"
@inline(never)
func testit(_ p: P) {
  p.foo()
}

public func callit(s: S) {
  testit(s)
}
