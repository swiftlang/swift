// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -parse-as-library -sil-serialize-all -o %t %s
// RUN: llvm-bcanalyzer %t/serialize_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -enable-sil-verify-all %t/serialize_attr.swiftmodule | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

// @_semantics
// -----------------------------------------------------------------------------

//CHECK-DAG: @_semantics("crazy") func foo()
@_semantics("crazy") func foo() -> Int  { return 5}

// @_specialize
// -----------------------------------------------------------------------------

// These lines should be contiguous.
// CHECK-DAG: @_specialize(Int, Float)
// CHECK-DAG: func specializeThis<T, U>(_ t: T, u: U)
@_specialize(Int, Float)
func specializeThis<T, U>(_ t: T, u: U) {}

protocol PP {
  associatedtype PElt
}
protocol QQ {
  associatedtype QElt
}

struct RR : PP {
  typealias PElt = Float
}
struct SS : QQ {
  typealias QElt = Int
}

struct GG<T : PP> {}

// These three lines should be contiguous, however, there is no way to
// sequence FileCheck directives while using CHECK-DAG as the outer
// label, and the declaration order is unpredictable.
//
// CHECK-DAG: class CC<T : PP> {
// CHECK-DAG: @_specialize(RR, SS)
// CHECK-DAG: @inline(never) func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>)
class CC<T : PP> {
  @inline(never)
  @_specialize(RR, SS)
  func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-DAG: sil hidden [fragile] [_specialize <Int, Float>] @_TF14serialize_attr14specializeThisu0_rFTx1uq__T_ : $@convention(thin) <T, U> (@in T, @in U) -> () {

// CHECK-DAG: sil hidden [fragile] [noinline] [_specialize <RR, Float, SS, Int>] @_TFC14serialize_attr2CC3foouRd__S_2QQrfTqd__1gGVS_2GGx__Tqd__GS2_x__ : $@convention(method) <T where T : PP><U where U : QQ> (@in U, GG<T>, @guaranteed CC<T>) -> (@out U, GG<T>) {
