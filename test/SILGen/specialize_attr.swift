// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | FileCheck %s

// CHECK-LABEL: @_specialize(Int, Float)
// CHECK-NEXT: func specializeThis<T, U>(_ t: T, u: U)
@_specialize(Int, Float)
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

// CHECK-LABEL: public class CC<T : PP> {
// CHECK-NEXT: @_specialize(RR, SS)
// CHECK-NEXT: @inline(never) public func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>)
public class CC<T : PP> {
  @inline(never)
  @_specialize(RR, SS)
  public func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-LABEL: sil hidden [_specialize <Int, Float>] @_TF15specialize_attr14specializeThisu0_rFTx1uq__T_ : $@convention(thin) <T, U> (@in T, @in U) -> () {

// CHECK-LABEL: sil [noinline] [_specialize <RR, Float, SS, Int>] @_TFC15specialize_attr2CC3foouRd__S_2QQrfTqd__1gGVS_2GGx__Tqd__GS2_x__ : $@convention(method) <T where T : PP><U where U : QQ> (@in U, GG<T>, @guaranteed CC<T>) -> (@out U, GG<T>) {
