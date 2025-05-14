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
// CHECK:       // %0 "p"
// CHECK:       } // end sil function '$s4main6testityyAA1P_pFTf4e_nAA1SV_Tg5Tf4x_n'
@inline(never)
func testit(_ p: P) {
  p.foo()
}

public func callit(s: S) {
  testit(s)
}

// Check that FSO does not trigger a verifier error caused by a mutated @in argument which is 
// converted to an @in_guaranteed argument (which must not be mutated).

public protocol IP<Element> {
  associatedtype Element

  init<Iterator>(iterator: consuming Iterator) where Iterator: IteratorProtocol, Iterator.Element == Element
}

extension Array: IP {
  public init<Iterator>(iterator: consuming Iterator) where Iterator: IteratorProtocol, Iterator.Element == Element {
    self.init()
    while let next = iterator.next() {
      append(next)
    }
  }
}
