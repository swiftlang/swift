// RUN: %target-swift-emit-silgen -module-name specialize_attr -emit-verbose-sil %s | %FileCheck %s
// RUN: %target-swift-emit-sil -sil-verify-all -O -module-name specialize_attr -emit-verbose-sil %s | %FileCheck -check-prefix=CHECK-OPT %s

// CHECK: @_specialize(exported: false, kind: full, where T == Int, U == Float)
// CHECK-NEXT: @_specialize(exported: false, kind: full, where T == Klass1, U == FakeString)
// CHECK-NEXT: func specializeThis<T, U>(_ t: T, u: U)
@_specialize(where T == Int, U == Float)
@_specialize(where T == Klass1, U == FakeString)
public func specializeThis<T, U>(_ t: T, u: U) {}

// CHECK: @_specialize(exported: false, kind: full, where T == Klass1, U == FakeString)
// CHECK-NEXT: func specializeThis2<T, U>(_ t: __owned T, u: __owned U) -> (T, U)
@_specialize(where T == Klass1, U == FakeString)
public func specializeThis2<T, U>(_ t: __owned T, u: __owned U) -> (T, U) {
    (t, u)
}

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

public class Klass1 {}
public class FakeStringData {}
public struct FakeString {
  var f: FakeStringData? = nil
}

public struct RRNonTrivial : PP {
  public typealias PElt = Klass1
  var elt: Klass1? = nil
}
public struct SSNonTrivial : QQ {
  public typealias QElt = FakeString
  var elt: FakeString? = nil
}

public struct GG<T : PP> {
  var t: T
}

// CHECK-LABEL: public class CC<T> where T : PP {
public class CC<T : PP> {
  // CHECK-NEXT: var k: Klass1?
  // To make non-trivial
  var k: Klass1? = nil

  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RR, U == SS)
  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RRNonTrivial, U == SSNonTrivial)
  // CHECK-NEXT: @inline(never) public func foo<U>(_ u: U, g: GG<T>) -> (U, GG<T>) where U : QQ
  @inline(never)
  @_specialize(where T == RR, U == SS)
  @_specialize(where T == RRNonTrivial, U == SSNonTrivial)
  public func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }

  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RR, U == SS)
  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RRNonTrivial, U == SSNonTrivial)
  // CHECK-NEXT: @inline(never) public func foo2<U>(_ u: __owned U, g: __owned GG<T>) -> (U, GG<T>) where U : QQ
  @inline(never)
  @_specialize(where T == RR, U == SS)
  @_specialize(where T == RRNonTrivial, U == SSNonTrivial)
  public func foo2<U : QQ>(_ u: __owned U, g: __owned GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-LABEL: public struct CC2<T> where T : PP {
public struct CC2<T : PP> {
  // CHECK-NEXT: var k: Klass1?
  // To make non-trivial
  var k: Klass1? = nil

  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RR, U == SS)
  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RRNonTrivial, U == SSNonTrivial)
  // CHECK-NEXT: @inline(never) public mutating func foo<U>(_ u: U, g: GG<T>) -> (U, GG<T>) where U : QQ
  @inline(never)
  @_specialize(where T == RR, U == SS)
  @_specialize(where T == RRNonTrivial, U == SSNonTrivial)
  mutating public func foo<U : QQ>(_ u: U, g: GG<T>) -> (U, GG<T>) {
    return (u, g)
  }

  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RR, U == SS)
  // CHECK-NEXT: @_specialize(exported: false, kind: full, where T == RRNonTrivial, U == SSNonTrivial)
  // CHECK-NEXT: @inline(never) public mutating func foo2<U>(_ u: __owned U, g: __owned GG<T>) -> (U, GG<T>) where U : QQ
  @inline(never)
  @_specialize(where T == RR, U == SS)
  @_specialize(where T == RRNonTrivial, U == SSNonTrivial)
  mutating public func foo2<U : QQ>(_ u: __owned U, g: __owned GG<T>) -> (U, GG<T>) {
    return (u, g)
  }
}

// CHECK-LABEL: sil [_specialize exported: false, kind: full, where T == Klass1, U == FakeString] [_specialize exported: false, kind: full, where T == Int, U == Float] [ossa] @$s15specialize_attr0A4This_1uyx_q_tr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @in_guaranteed U) -> () {

// CHECK-OPT-LABEL: sil shared [noinline] @$s15specialize_attr2CCC3foo_1gqd___AA2GGVyxGtqd___AHtAA2QQRd__lFAA12RRNonTrivialV_AA05SSNonH0VTg5 : $@convention(method) (@guaranteed SSNonTrivial, @guaranteed GG<RRNonTrivial>, @guaranteed CC<RRNonTrivial>) -> (@owned SSNonTrivial, @out GG<RRNonTrivial>) {

// CHECK-OPT-LABEL: sil shared [noinline] @$s15specialize_attr2CCC4foo2_1gqd___AA2GGVyxGtqd__n_AHntAA2QQRd__lFAA2RRV_AA2SSVTg5 : $@convention(method) (SS, GG<RR>, @guaranteed CC<RR>) -> (SS, @out GG<RR>) {

// CHECK-OPT-LABEL: sil [noinline] @$s15specialize_attr2CCC4foo2_1gqd___AA2GGVyxGtqd__n_AHntAA2QQRd__lF : $@convention(method) <T where T : PP><U where U : QQ> (@in U, @in GG<T>, @guaranteed CC<T>) -> (@out U, @out GG<T>) {

// CHECK-LABEL: sil [noinline] [_specialize exported: false, kind: full, where T == RRNonTrivial, U == SSNonTrivial] [_specialize exported: false, kind: full, where T == RR, U == SS] [ossa] @$s15specialize_attr2CCC3foo_1gqd___AA2GGVyxGtqd___AHtAA2QQRd__lF : $@convention(method) <T where T : PP><U where U : QQ> (@in_guaranteed U, @in_guaranteed GG<T>, @guaranteed CC<T>) -> (@out U, @out GG<T>) {

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
    @_specialize(where Element == Klass1)
    get {
      return storage[i]
    }

    @_specialize(where Element == Int)
    @_specialize(where Element == Klass1)
    set(rhs) {
      storage[i] = rhs
    }
  }
}

// ASubscriptable.subscript.getter with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Klass1] [_specialize exported: false, kind: full, where Element == Int] [ossa] @$s15specialize_attr14ASubscriptableCyxSicig : $@convention(method) <Element> (Int, @guaranteed ASubscriptable<Element>) -> @out Element {

// ASubscriptable.subscript.setter with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Klass1] [_specialize exported: false, kind: full, where Element == Int] [ossa] @$s15specialize_attr14ASubscriptableCyxSicis : $@convention(method) <Element> (@in Element, Int, @guaranteed ASubscriptable<Element>) -> () {

// ASubscriptable.subscript.modify with no attribute
// CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s15specialize_attr14ASubscriptableCyxSiciM : $@yield_once @convention(method) <Element> (Int, @guaranteed ASubscriptable<Element>) -> @yields @inout Element {

public class Addressable<Element> : TestSubscriptable {
  var storage: UnsafeMutablePointer<Element>

  init(capacity: Int) {
    storage = UnsafeMutablePointer<Element>.allocate(capacity: capacity)
  }

  public subscript(i: Int) -> Element {
    @_specialize(where Element == Int)
    @_specialize(where Element == Klass1)
    unsafeAddress {
      return UnsafePointer<Element>(storage + i)
    }

    @_specialize(where Element == Int)
    @_specialize(where Element == Klass1)
    unsafeMutableAddress {
      return UnsafeMutablePointer<Element>(storage + i)
    }
  }
}

// Addressable.subscript.unsafeAddressor with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Klass1] [_specialize exported: false, kind: full, where Element == Int] [ossa] @$s15specialize_attr11AddressableCyxSicilu : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> UnsafePointer<Element> {

// Addressable.subscript.unsafeMutableAddressor with _specialize
// CHECK-LABEL: sil [_specialize exported: false, kind: full, where Element == Klass1] [_specialize exported: false, kind: full, where Element == Int] [ossa] @$s15specialize_attr11AddressableCyxSiciau : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> UnsafeMutablePointer<Element> {

// Addressable.subscript.getter with no attribute
// CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s15specialize_attr11AddressableCyxSicig : $@convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> @out Element {

// Addressable.subscript.setter with no attribute
// CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s15specialize_attr11AddressableCyxSicis : $@convention(method) <Element> (@in Element, Int, @guaranteed Addressable<Element>) -> () {

// Addressable.subscript.modify with no attribute
// CHECK-LABEL: sil [transparent] [serialized] [ossa] @$s15specialize_attr11AddressableCyxSiciM : $@yield_once @convention(method) <Element> (Int, @guaranteed Addressable<Element>) -> @yields @inout Element {

