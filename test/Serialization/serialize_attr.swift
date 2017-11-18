// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -parse-as-library -o %t %s
// RUN: llvm-bcanalyzer %t/serialize_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -enable-sil-verify-all -disable-sil-linking %t/serialize_attr.swiftmodule | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

// @_semantics
// -----------------------------------------------------------------------------

//CHECK-DAG: @_semantics("crazy") func foo()
@_inlineable
@_versioned
@_semantics("crazy") func foo() -> Int  { return 5}

// @_optimize
// -----------------------------------------------------------------------------

//CHECK-DAG: @_optimize(none) func test_onone()
@_inlineable
@_versioned
@_optimize(none)
func test_onone() -> Int  { return 5}

//CHECK-DAG: @_optimize(speed) func test_ospeed()
@_inlineable
@_versioned
@_optimize(speed)
func test_ospeed() -> Int  { return 5}
 
//CHECK-DAG: @_optimize(size) func test_osize()
@_inlineable
@_versioned
@_optimize(size)
func test_osize() -> Int  { return 5}

// @_specialize
// -----------------------------------------------------------------------------

// These lines should be contiguous.
// CHECK-DAG: @_specialize(exported: false, kind: full, where T == Int, U == Float)
// CHECK-DAG: func specializeThis<T, U>(_ t: T, u: U)
@_inlineable
@_versioned
@_specialize(where T == Int, U == Float)
func specializeThis<T, U>(_ t: T, u: U) {}

public protocol PP {
  associatedtype PElt
}
public protocol QQ {
  associatedtype QElt
}

public struct RR : PP {
  public typealias PElt = Float
}
public struct SS : QQ {
  public typealias QElt = Int
}

public struct GG<T : PP> {}

// These three lines should be contiguous, however, there is no way to
// sequence FileCheck directives while using CHECK-DAG as the outer
// label, and the declaration order is unpredictable.
//
// CHECK-DAG: class CC<T> where T : PP {
// CHECK-DAG: @_specialize(exported: false, kind: full, where T == RR, U == SS)
// CHECK-DAG: @inline(never) func foo<U>(_ u: U, g: GG<T>) -> (U, GG<T>) where U : QQ
public class CC<T : PP> {
  @_inlineable
  @_versioned
  @inline(never)
  @_specialize(where T==RR, U==SS)
  func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-DAG: sil [serialized] [_specialize exported: false, kind: full, where T == Int, U == Float] @_T014serialize_attr14specializeThisyx_q_1utr0_lF : $@convention(thin) <T, U> (@in T, @in U) -> () {

// CHECK-DAG: sil [serialized] [noinline] [_specialize exported: false, kind: full, where T == RR, U == SS] @_T014serialize_attr2CCC3fooqd___AA2GGVyxGtqd___AG1gtAA2QQRd__lF : $@convention(method) <T where T : PP><U where U : QQ> (@in U, GG<T>, @guaranteed CC<T>) -> (@out U, GG<T>) {
