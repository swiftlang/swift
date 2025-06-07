// RUN: %target-swift-frontend -parse-as-library -module-name test %s -O -Xllvm -sil-print-types -emit-sil | %FileCheck %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

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

let globalLetArray = [1, 2, 3, 4]

// CHECK-LABEL: sil @$s4test5test15indexS2i_tF :
// CHECK:           [[L:%.*]] = load
// CHECK-NOT:       load
// CHECK:           builtin "sadd_with_overflow{{.*}}"([[L]] : {{.*}}, [[L]] :
// CHECK:       } // end sil function '$s4test5test15indexS2i_tF'
public func test1(index: Int) -> Int {
  let elem1 = globalLetArray[index]
  let elem2 = globalLetArray[index]
  return elem1 + elem2
}

struct Wrapper {
  let arr: Array<Int>
  init() {
    arr = [1, 2, 3, 4]
  }
}

// CHECK-LABEL: sil @$s4test5test25indexS2i_tF :
// CHECK:           [[L:%.*]] = load
// CHECK-NOT:       load
// CHECK:           builtin "sadd_with_overflow{{.*}}"([[L]] : {{.*}}, [[L]] :
// CHECK:       } // end sil function '$s4test5test25indexS2i_tF'
public func test2(index: Int) -> Int {
  let w = Wrapper()
  let elem1 = w.arr[index]
  let elem2 = w.arr[index]
  return elem1 + elem2
}

// CHECK-LABEL: sil @$s4test5test3_5indexSiSaySiG_SitF :
// CHECK:           load
// CHECK:           [[L:%.*]] = load
// CHECK-NOT:       load
// CHECK:           builtin "sadd_with_overflow{{.*}}"([[L]] : {{.*}}, [[L]] :
// CHECK:       } // end sil function '$s4test5test3_5indexSiSaySiG_SitF'
public func test3(_ arr: Array<Int>, index: Int) -> Int {
  return arr[index] + arr[index]
}

