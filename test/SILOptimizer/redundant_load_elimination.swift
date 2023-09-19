// RUN: %target-swift-frontend -parse-as-library -module-name test %s -O -emit-sil | %FileCheck %s

// REQUIRES: swift_in_compiler


public final class C {
  let i: Int

  init(i: Int) {
    self.i = i
  }
}

// CHECK-LABEL: sil @$s4test0A8LetField_1fSi_SitAA1CC_yyXEtF :
// CHECK:           [[A:%.*]] = ref_element_addr [immutable] %0 : $C, #C.i
// CHECK:           [[L:%.*]] = load [[A]]
// CHECK:           apply
// CHECK-NOT:       load
// CHECK:           tuple ([[L]] : $Int, [[L]] : $Int)
// CHECK:       } // end sil function '$s4test0A8LetField_1fSi_SitAA1CC_yyXEtF'
public func testLetField(_ c: C, f: () -> ()) ->  (Int, Int) {
  let a = c.i
  f()
  let b = c.i
  return (a, b)
}

