// RUN: rm -rf %t
// RUN: mkdir -p %t
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
// CHECK-DAG: @_specialize(exported: false, kind: full, where T == Int, U == Float)
// CHECK-DAG: func specializeThis<T, U>(_ t: T, u: U)
@_specialize(where T == Int, U == Float)
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
// CHECK-DAG: class CC<T> where T : PP {
// CHECK-DAG: @_specialize(exported: false, kind: full, where T == RR, U == SS)
// CHECK-DAG: @inline(never) func foo<U>(_ u: U, g: GG<T>) -> (U, GG<T>) where U : QQ
class CC<T : PP> {
  @inline(never)
  @_specialize(where T==RR, U==SS)
  func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-DAG: sil hidden [serialized] [_specialize exported: false, kind: full, where T == Int, U == Float] @_T014serialize_attr14specializeThisyx_q_1utr0_lF : $@convention(thin) <T, U> (@in T, @in U) -> () {

// CHECK-DAG: sil hidden [serialized] [noinline] [_specialize exported: false, kind: full, where T == RR, U == SS] @_T014serialize_attr2CCC3fooqd___AA2GGVyxGtqd___AG1gtAA2QQRd__lF : $@convention(method) <T where T : PP><U where U : QQ> (@in U, GG<T>, @guaranteed CC<T>) -> (@out U, GG<T>) {
