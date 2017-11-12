// RUN: %target-swift-frontend -parse-as-library -O -module-name=test %s -emit-sil | %FileCheck %s
// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib
// XFAIL: linux
// rdar://problem/34758773


// This is an end-to-end test to check if the array literal in the loop is
// stack promoted.

// CHECK-LABEL: sil @{{.*}}testit
// CHECK:  alloc_ref [stack] [tail_elems

public func testit(_ N: Int) {
  for _ in 0..<N {
    for _ in 0..<10 {
       var nums = [Int]()
       for _ in 0..<40_000 {
         nums += [1, 2, 3, 4, 5, 6, 7]
       }
    }
  }
}
