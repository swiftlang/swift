// RUN: %target-swift-frontend -O -emit-sil %s | %target-sil-opt -assume-parsing-unqualified-ownership-sil -simplify-unreachable-containing-blocks | %FileCheck %s

// Make sure we can swap two values in an array without retaining anything along non-fatalerror paths.

// CHECK-LABEL: sil @_T011swap_refcnt0A7ByIndexySays4Int8VGz1A_Si1xSi1ytF : $@convention(thin) (@inout Array<Int8>, Int, Int) -> () {
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
// CHECK: } // end sil function '_T011swap_refcnt0A7ByIndexySays4Int8VGz1A_Si1xSi1ytF'
public func swapByIndex(A: inout [Int8], x : Int, y : Int) {
  swap(&A[x],&A[y])
}

