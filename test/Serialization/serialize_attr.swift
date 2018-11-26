
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend  -module-name serialize_attr -emit-module -parse-as-library -o %t %s
// RUN: llvm-bcanalyzer %t/serialize_attr.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-sil-opt -enable-sil-verify-all -disable-sil-linking %t/serialize_attr.swiftmodule | %FileCheck %s

// BCANALYZER-NOT: UnknownCode

// @_semantics
// -----------------------------------------------------------------------------

//CHECK-DAG: @_semantics("crazy") func foo()
@inlinable
@_semantics("crazy") func foo() -> Int  { return 5}

// @_optimize
// -----------------------------------------------------------------------------

//CHECK-DAG: @_optimize(none) func test_onone()
@inlinable
@_optimize(none)
func test_onone() -> Int  { return 5}

//CHECK-DAG: @_optimize(speed) func test_ospeed()
@inlinable
@_optimize(speed)
func test_ospeed() -> Int  { return 5}
 
//CHECK-DAG: @_optimize(size) func test_osize()
@inlinable
@_optimize(size)
func test_osize() -> Int  { return 5}

// @_specialize
// -----------------------------------------------------------------------------

// These lines should be contiguous.
// CHECK-DAG: @_specialize(exported: false, kind: full, where T == Int, U == Float)
// CHECK-DAG: func specializeThis<T, U>(_ t: T, u: U)
@inlinable
@_specialize(where T == Int, U == Float)
func specializeThis<T, U>(_ t: T, u: U) {
  specializeThat(t, u: u)
}

@usableFromInline
@_specialize(where T == Int, U == Float)
func specializeThat<T, U>(_ t: T, u: U) {}

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
  @inlinable
  @inline(never)
  @_specialize(where T==RR, U==SS)
  func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-DAG: sil [serialized] [_specialize exported: false, kind: full, where T == Int, U == Float] [canonical] @$s14serialize_attr14specializeThis_1uyx_q_tr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @in_guaranteed U) -> () {

// CHECK-DAG: sil [serialized] [noinline] [_specialize exported: false, kind: full, where T == RR, U == SS] [canonical] @$s14serialize_attr2CCC3foo_1gqd___AA2GGVyxGtqd___AHtAA2QQRd__lF : $@convention(method) <T where T : PP><U where U : QQ> (@in_guaranteed U, GG<T>, @guaranteed CC<T>) -> (@out U, GG<T>) {
