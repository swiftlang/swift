// RUN: %target-swift-frontend -O -Xllvm -sil-disable-pass=function-signature-opts -emit-sil -primary-file %s | %FileCheck %s

public struct S {
  @inline(never)
  init<T>(_ t: T) {
    fatalError()
  }
}

// check that this doesn't cause a memory lifetime failure

// CHECK-LABEL: sil @$s22generic_specialization6testityAA1SVSSF :
// CHECK:          [[F:%.*]] = function_ref @$s22generic_specialization1SVyACxclufCSS_Tt0g5 :
// CHECK:            = apply [[F]](%0) : $@convention(thin) (@owned String) -> S
// CHECK:       } // end sil function '$s22generic_specialization6testityAA1SVSSF'
public func testit(_ s: String) -> S {
  return S(s)
}

