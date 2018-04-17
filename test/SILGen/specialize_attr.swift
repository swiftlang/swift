
// RUN: %target-swift-frontend -module-name specialize_attr -emit-silgen -enable-sil-ownership -emit-verbose-sil %s | %FileCheck %s

// CHECK-LABEL: @_specialize(exported: false, kind: full, where T == Int, U == Float)
// CHECK-NEXT: func specializeThis<T, U>(_ t: T, u: U)
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

// CHECK-LABEL: public class CC<T> where T : PP {
// CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RR, U == SS)
// CHECK-NEXT: @inline(never) public func foo<U>(_ u: U, g: GG<T>) -> (U, GG<T>) where U : QQ
public class CC<T : PP> {
  @inline(never)
  @_specialize(where T == RR, U == SS)
  public func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-LABEL: sil hidden [_specialize exported: false, kind: full, where T == Int, U == Float] @$S15specialize_attr0A4This_1uyx_q_tr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @in_guaranteed U) -> () {

// CHECK-LABEL: sil [noinline] [_specialize exported: false, kind: full, where T == RR, U == SS] @$S15specialize_attr2CCC3foo_1gqd___AA2GGVyxGtqd___AHtAA2QQRd__lF : $@convention(method) <T where T : PP><U where U : QQ> (@in_guaranteed U, GG<T>, @guaranteed CC<T>) -> (@out U, GG<T>) {

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
    @_specialize(where Element == Int)
    get {
      return storage[i]
    }
    @_specialize(where Element == Int)
    set(rhs) {
      storage[i] = rhs
    }
  }
}

// ASubscriptable.subscript.getter with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Int] @$S15specialize_attr14ASubscriptableCyxSicig : $@convention(method) <Element> (Int, @guaranteed ASubscriptable<Element>) -> @out Element {

// ASubscriptable.subscript.setter with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Int] @$S15specialize_attr14ASubscriptableCyxSicis : $@convention(method) <Element> (@in Element, Int, @guaranteed ASubscriptable<Element>) -> () {

// ASubscriptable.subscript.materializeForSet with no attribute
// CHECK-LABEL: sil [transparent] [serialized] @$S15specialize_attr14ASubscriptableCyxSicim : $@convention(method) <Element> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, Int, @guaranteed ASubscriptable<Element>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {

public class Addressable<Element> : TestSubscriptable {
  var storage: UnsafeMutablePointer<Element>

  init(capacity: Int) {
    storage = UnsafeMutablePointer<Element>.allocate(capacity: capacity)
  }

  public subscript(i: Int) -> Element {
    @_specialize(where Element == Int)
    unsafeAddress {
      return UnsafePointer<Element>(storage + i)
    }
    @_specialize(where Element == Int)
    unsafeMutableAddress {
      return UnsafeMutablePointer<Element>(storage + i)
    }
  }
}

// Addressable.subscript.getter with no attribute
// CHECK-LABEL: sil [transparent] [serialized] @$S15specialize_attr11AddressableCyxSicig : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> @out Element {

// Addressable.subscript.unsafeAddressor with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Int] @$S15specialize_attr11AddressableCyxSicilu : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> UnsafePointer<Element> {

// Addressable.subscript.setter with no attribute
// CHECK-LABEL: sil [transparent] [serialized] @$S15specialize_attr11AddressableCyxSicis : $@convention(method) <Element> (@in Element, Int, @guaranteed Addressable<Element>) -> () {

// Addressable.subscript.unsafeMutableAddressor with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Int] @$S15specialize_attr11AddressableCyxSiciau : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> UnsafeMutablePointer<Element> {

// Addressable.subscript.materializeForSet with no attribute
// CHECK-LABEL: sil [transparent] [serialized] @$S15specialize_attr11AddressableCyxSicim : $@convention(method) <Element> (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, Int, @guaranteed Addressable<Element>) -> (Builtin.RawPointer, Optional<Builtin.RawPointer>) {

