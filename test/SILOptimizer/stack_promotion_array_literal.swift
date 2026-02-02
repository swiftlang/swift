// RUN: %target-swift-frontend -parse-as-library -O -module-name=test %s -emit-sil | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// REQUIRES: swift_in_compiler

// This is an end-to-end test to check if the array literal in the loop is
// stack promoted.

// CHECK-LABEL: sil @$s4test6testityySi_SitF :
// CHECK:         alloc_ref{{.*}} [stack] [tail_elems
// CHECK:       } // end sil function '$s4test6testityySi_SitF'
public func testit(_ N: Int, _ x: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       for _ in 0..<40_000 {
         nums += [1, 2, 3, 4, 5, 6, x]
       }
    }
  }
}

public protocol Proto {
  func at() -> Int
}

// CHECK-LABEL: sil @$s4test5test2ySiAA5Proto_pF :
// CHECK:         alloc_ref{{.*}} [stack] [tail_elems $any Proto
// CHECK:         br bb1
// CHECK:       bb1({{.*}}):
// CHECK:         [[M:%.*]] = witness_method
// CHECK:         apply [[M]]
// CHECK:         cond_br
// CHECK:       bb2:
// CHECK:       } // end sil function '$s4test5test2ySiAA5Proto_pF'
public func test2(_ p: Proto) -> Int {
  var a = [p, p, p]
  var b = 0
  a.withUnsafeMutableBufferPointer {
    let array = $0
    for i in 0..<array.count {
      b += array[i].at()
    }
  }
  return b
}
