// RUN: %target-swift-frontend -emit-silgen -emit-verbose-sil %s | %FileCheck %s

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

// CHECK-LABEL: public class CC<T> where T : PP {
// CHECK-NEXT: @_specialize(RR, SS)
// CHECK-NEXT: @inline(never) public func foo<U>(_ u: U, g: GG<T>) -> (U, GG<T>) where U : QQ
public class CC<T : PP> {
  @inline(never)
  @_specialize(RR, SS)
  public func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-LABEL: sil hidden [_specialize <Int, Float>] @_TF15specialize_attr14specializeThisu0_rFTx1uq__T_ : $@convention(thin) <T, U> (@in T, @in U) -> () {

// CHECK-LABEL: sil [noinline] [_specialize <RR, SS>] @_TFC15specialize_attr2CC3foouRd__S_2QQrfTqd__1gGVS_2GGx__Tqd__GS2_x__ : $@convention(method) <T where T : PP><U where U : QQ> (@in U, GG<T>, @guaranteed CC<T>) -> (@out U, GG<T>) {

// -----------------------------------------------------------------------------
// Test user-specialized subscript accessors.

public protocol TestSubscriptable {
  associatedtype Element
  subscript(i: Int) -> Element { get set }
}

public class ASubscriptable<Element> : TestSubscriptable {
  var storage: UnsafeMutablePointer<Element>

  init(capacity: Int) {
    storage = UnsafeMutablePointer<Element>.allocate(capacity: capacity)
  }

  public subscript(i: Int) -> Element {
    @_specialize(Int)
    get {
      return storage[i]
    }
    @_specialize(Int)
    set(rhs) {
      storage[i] = rhs
    }
  }
}

// ASubscriptable.subscript.getter with _specialize
// CHECK-LABEL: sil [_specialize <Int>] @_TFC15specialize_attr14ASubscriptableg9subscriptFSix : $@convention(method) <Element> (Int, @guaranteed ASubscriptable<Element>) -> @out Element {

// ASubscriptable.subscript.setter with _specialize
// CHECK-LABEL: sil [_specialize <Int>] @_TFC15specialize_attr14ASubscriptables9subscriptFSix : $@convention(method) <Element> (@in Element, Int, @guaranteed ASubscriptable<Element>) -> () {

// ASubscriptable.subscript.materializeForSet with no attribute
// CHECK-LABEL: sil [transparent] [fragile] @_TFC15specialize_attr14ASubscriptablem9subscriptFSix : $@convention(method) <Element> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, Int, @guaranteed ASubscriptable<Element>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {

public class Addressable<Element> : TestSubscriptable {
  var storage: UnsafeMutablePointer<Element>

  init(capacity: Int) {
    storage = UnsafeMutablePointer<Element>.allocate(capacity: capacity)
  }

  public subscript(i: Int) -> Element {
    @_specialize(Int)
    unsafeAddress {
      return UnsafePointer<Element>(storage + i)
    }
    @_specialize(Int)
    unsafeMutableAddress {
      return UnsafeMutablePointer<Element>(storage + i)
    }
  }
}

// Addressable.subscript.unsafeAddressor with _specialize
// CHECK-LABEL: sil [_specialize <Int>] @_TFC15specialize_attr11Addressablelu9subscriptFSix : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> UnsafePointer<Element> {

// Addressable.subscript.unsafeMutableAddressor with _specialize
// CHECK-LABEL: sil [_specialize <Int>] @_TFC15specialize_attr11Addressableau9subscriptFSix : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> UnsafeMutablePointer<Element> {


// Addressable.subscript.getter with no attribute
// CHECK-LABEL: sil [transparent] [fragile] @_TFC15specialize_attr11Addressableg9subscriptFSix : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> @out Element {

// Addressable.subscript.setter with no attribute
// CHECK-LABEL: sil [transparent] [fragile] @_TFC15specialize_attr11Addressables9subscriptFSix : $@convention(method) <Element> (@in Element, Int, @guaranteed Addressable<Element>) -> () {

// Addressable.subscript.materializeForSet with no attribute
// CHECK-LABEL: sil [transparent] [fragile] @_TFC15specialize_attr11Addressablem9subscriptFSix : $@convention(method) <Element> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, Int, @guaranteed Addressable<Element>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {
