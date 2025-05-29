// Test .swiftmodule with library-evolution
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name A -emit-module-path %t/A.swiftmodule -enable-library-evolution -swift-version 5 %S/Inputs/specialize_attr_module.swift
// RUN: %target-swift-frontend -I %t -module-name B -emit-module-path %t/B.swiftmodule -enable-library-evolution -swift-version 5 %S/Inputs/specialize_attr_module2.swift
// RUN: %target-swift-emit-silgen -I %t -module-name specialize_attr -emit-verbose-sil %s -swift-version 5 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-os
// RUN: %target-swift-emit-silgen -I %t -module-name specialize_attr -emit-verbose-sil %s -swift-version 5 | %FileCheck %s -check-prefix=CHECK2
// RUN: %target-swift-emit-sil -I %t -sil-verify-all -O -module-name specialize_attr -emit-verbose-sil %s | %FileCheck -check-prefix=CHECK-OPT -check-prefix=CHECK-OPT-EVO -check-prefix=CHECK-OPT-%target-os %s

// Test .swiftinterface
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o /dev/null -module-name A -emit-module-interface-path %t/A.swiftinterface -enable-library-evolution -swift-version 5 %S/Inputs/specialize_attr_module.swift
// RUN: %target-swift-frontend -emit-module -o /dev/null -I %t -module-name B -emit-module-interface-path %t/B.swiftinterface -enable-library-evolution -swift-version 5 %S/Inputs/specialize_attr_module2.swift
// RUN: %target-swift-emit-silgen -I %t -module-name specialize_attr -emit-verbose-sil %s -swift-version 5 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-os
// RUN: %target-swift-emit-sil -I %t -sil-verify-all -O -module-name specialize_attr -emit-verbose-sil %s | %FileCheck -check-prefix=CHECK-OPT -check-prefix=CHECK-OPT-EVO %s

// Test .swiftmodule without library-evolution
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name A -emit-module-path %t/A.swiftmodule -swift-version 5 %S/Inputs/specialize_attr_module.swift
// RUN: %target-swift-frontend -I %t -module-name B -emit-module-path %t/B.swiftmodule -swift-version 5 %S/Inputs/specialize_attr_module2.swift
// RUN: %target-swift-emit-silgen -I %t -module-name specialize_attr -emit-verbose-sil %s -swift-version 5 | %FileCheck %s -check-prefix=CHECK -check-prefix=CHECK-%target-os
// RUN: %target-swift-emit-sil -I %t -sil-verify-all -O -module-name specialize_attr -emit-verbose-sil %s | %FileCheck -check-prefix=CHECK-OPT -check-prefix=CHECK-OPT-NOEVO %s

import A
import B

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

// Import from module A and B.
// CHECK-OPT-macosx-DAG: sil {{.*}} [_specialize exported: true, kind: full, where T == Double] [_specialize exported: true, kind: full, available: 10.50, where T == Int16] [_specialize exported: true, kind: full, where T == Int] @$s1A11PublicThingV11doStuffWithyyxF : $@convention(method) <T> (@in_guaranteed T, PublicThing<T>) -> ()
// CHECK-OPT-linux-gnu-DAG: sil {{.*}} [_specialize exported: true, kind: full, where T == Double] [_specialize exported: true, kind: full, where T == Int16] [_specialize exported: true, kind: full, where T == Int] @$s1A11PublicThingV11doStuffWithyyxF : $@convention(method) <T> (@in_guaranteed T, PublicThing<T>) -> ()
// CHECK-OPT-DAG: sil {{.*}} [_specialize exported: true, kind: full, where T == Double] [_specialize exported: true, kind: full, where T == Int] @$s1A13InternalThingV11doStuffWith5boxedyAA05BoxedB0VyxG_tF : $@convention(method) <T> (BoxedThing<T>, InternalThing<T>) -> ()

// CHECK-OPT-DAG: sil {{.*}} [_specialize exported: true, kind: full, where T == Int] @$s1A14InternalThing2V9computedZxvM : $@yield_once @convention(method) <T> (@inout InternalThing2<T>) -> @yields @inout T
// CHECK-OPT-DAG: sil {{.*}} [_specialize exported: true, kind: full, where T == Int] @$s1A14InternalThing2V9computedZxvr : $@yield_once @convention(method) <T> (@in_guaranteed InternalThing2<T>) -> @yields @in_guaranteed T

// CHECK-OPT-DAG: sil {{.*}} [_specialize exported: true, kind: full, where T == Int] @$s1A14InternalThing2VyxSicig : $@convention(method) <T> (Int, @in_guaranteed InternalThing2<T>) -> @out T
// CHECK-OPT-DAG: sil {{.*}} [_specialize exported: true, kind: full, where T == Int] @$s1A14InternalThing2VyxSicis : $@convention(method) <T> (@in T, Int, @inout InternalThing2<T>) -> ()

// CHECK-LABEL: sil [_specialize exported: false, kind: full, where T == Klass1, U == FakeString] [_specialize exported: false, kind: full, where T == Int, U == Float] [ossa] @$s15specialize_attr0A4This_1uyx_q_tr0_lF : $@convention(thin) <T, U> (@in_guaranteed T, @in_guaranteed U) -> () {

// CHECK-OPT-DAG: sil shared [noinline] {{.*}}@$s15specialize_attr2CCC3foo_1gqd___AA2GGVyxGtqd___AHtAA2QQRd__lFAA12RRNonTrivialV_AA05SSNonH0VTg5 : $@convention(method) (@guaranteed SSNonTrivial, @guaranteed GG<RRNonTrivial>, @guaranteed CC<RRNonTrivial>) -> (@owned SSNonTrivial, @out GG<RRNonTrivial>) {

// CHECK-OPT-DAG: sil shared [noinline] {{.*}}@$s15specialize_attr2CCC4foo2_1gqd___AA2GGVyxGtqd__n_AHntAA2QQRd__lFAA2RRV_AA2SSVTg5 : $@convention(method) (SS, GG<RR>, @guaranteed CC<RR>) -> (SS, @out GG<RR>) {

// CHECK-OPT-DAG: sil [noinline] {{.*}}@$s15specialize_attr2CCC4foo2_1gqd___AA2GGVyxGtqd__n_AHntAA2QQRd__lF : $@convention(method) <T where T : PP><U where U : QQ> (@in U, @in GG<T>, @guaranteed CC<T>) -> (@out U, @out GG<T>) {

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


public struct TestPrespecialized<T> {
  // CHECK-LABEL: sil [_specialize exported: true, kind: full, where T == Double] [_specialize exported: true, kind: full, where T == Int] [ossa] @$s15specialize_attr18TestPrespecializedV5aFuncyyF : $@convention(method) <T> (TestPrespecialized<T>) -> () {
  @_specialize(exported: true, where T == Int)
  @_specialize(exported: true, where T == Double)
  public func aFunc() {}
}

extension TestPrespecialized {
  // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s15specialize_attr18TestPrespecializedV5aFuncyyF", where T == Int32] [ossa] @$s15specialize_attr18TestPrespecializedV0A6TargetyyF : $@convention(method) <T> (TestPrespecialized<T>) -> () {
  @_specialize(exported: true, kind: full, target: aFunc(), where T == Int32)
  public func specializeTarget() {}
}

extension PublicThing {
  // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A11PublicThingV11doStuffWithyyxF", where T == PublicPlainThing] [_specialize exported: true, kind: full, target: "$s1A11PublicThingV11doStuffWithyyxF", where T == Float] [ossa] @$s1A11PublicThingV15specialize_attrE17specializedoStuffyyxF : $@convention(method) <T> (@in_guaranteed T, PublicThing<T>) -> () {

  // CHECK-OPT-DAG: sil @$s1A11PublicThingV11doStuffWithyyxFAA0a5PlainB0V_Ts5 : $@convention(method) (PublicPlainThing, PublicThing<PublicPlainThing>) -> () {
  // CHECK-OPT-DAG: sil @$s1A11PublicThingV11doStuffWithyyxFSf_Ts5 : $@convention(method) (Float, PublicThing<Float>) -> () {

  @_specialize(exported: true, kind: full, target: doStuffWith(_:), where T == Float)
  @_specialize(exported: true, kind: full, target: doStuffWith(_:), where T == PublicPlainThing)
  public func specializedoStuff(_ t: T) {}
}

@_specializeExtension
extension InternalThing {

  // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A13InternalThingV11doStuffWithyyxF", where T == PublicPlainThing] [ossa] @$s1A13InternalThingV15specialize_attrE18specializedDoStuffyyxF : $@convention(method) <T> (@in_guaranteed T, InternalThing<T>) -> () {
  // CHECK-OPT-DAG: sil @$s1A13InternalThingV11doStuffWithyyxFAA011PublicPlainB0V_Ts5 : $@convention(method) (PublicPlainThing, InternalThing<PublicPlainThing>) -> () {

  @_specialize(exported: true, kind: full, target: doStuffWith(_:), where T == PublicPlainThing)
  public func specializedDoStuff(_ t: T) {}

  // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A13InternalThingV11doStuffWith5boxedyAA05BoxedB0VyxG_tF", where T == PublicPlainThing] [ossa] @$s1A13InternalThingV15specialize_attrE22specializedDoStuffWith5boxedyAA05BoxedB0VyxG_tF : $@convention(method) <T> (BoxedThing<T>, InternalThing<T>) -> () {
  // CHECK-OPT-DAG: sil @$s1A13InternalThingV11doStuffWith5boxedyAA05BoxedB0VyxG_tFAA011PublicPlainB0V_Ts5 : $@convention(method) (BoxedThing<PublicPlainThing>, InternalThing<PublicPlainThing>) -> () {

  @_specialize(exported: true, target:doStuffWith(boxed:), where T == PublicPlainThing)
  public func specializedDoStuffWith(boxed: BoxedThing<T>) {}

  // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A13InternalThingV11doStuffWith6boxed2yAA11BoxedThing2VyxG_tF", where T == ResilientThing] [_specialize exported: true, kind: full, target: "$s1A13InternalThingV11doStuffWith6boxed2yAA11BoxedThing2VyxG_tF", where T == Int16] [_specialize exported: true, kind: full, target: "$s1A13InternalThingV11doStuffWith6boxed2yAA11BoxedThing2VyxG_tF", where T == Klass1] [ossa] @$s1A13InternalThingV15specialize_attrE23specializedDoStuffWith26boxed2yAA11BoxedThing2VyxG_tF : $@convention(method) <T> (@in_guaranteed BoxedThing2<T>, InternalThing<T>) -> () {
  // CHECK-OPT-EVO-DAG: sil @$s1A13InternalThingV11doStuffWith6boxed2yAA11BoxedThing2VyxG_tFAA09ResilientB0V_Ts5 : $@convention(method) (@in_guaranteed BoxedThing2<ResilientThing>, InternalThing<ResilientThing>) -> () {
  // CHECK-OPT-NOEVO-DAG: sil @$s1A13InternalThingV11doStuffWith6boxed2yAA11BoxedThing2VyxG_tFAA09ResilientB0V_Ts5 : $@convention(method) (BoxedThing2<ResilientThing>, InternalThing<ResilientThing>) -> () {
  // CHECK-OPT-DAG: sil @$s1A13InternalThingV11doStuffWith6boxed2yAA11BoxedThing2VyxG_tFs5Int16V_Ts5 : $@convention(method) (BoxedThing2<Int16>, InternalThing<Int16>) -> () {
  // CHECK-OPT-DAG: sil @$s1A13InternalThingV11doStuffWith6boxed2yAA11BoxedThing2VyxG_tF15specialize_attr6Klass1C_Ts5 : $@convention(method) (@guaranteed BoxedThing2<Klass1>, InternalThing<Klass1>) -> () {

  @_specialize(exported: true, target:doStuffWith(boxed2:), where T == Klass1)
  @_specialize(exported: true, target:doStuffWith(boxed2:), where T == Int16)
  @_specialize(exported: true, target:doStuffWith(boxed2:), where T == ResilientThing)
  public func specializedDoStuffWith2(boxed2: BoxedThing2<T>) {}
}


@_specializeExtension
extension InternalThing2 {

  public var specializeComputedX : T {

    // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A14InternalThing2V9computedXxvg", where T == Klass1] [ossa] @$s1A14InternalThing2V15specialize_attrE0C9ComputedXxvg : $@convention(method) <T> (@in_guaranteed InternalThing2<T>) -> @out T {
    // CHECK-OPT-DAG: sil {{.*}}@$s1A14InternalThing2V9computedXxvg15specialize_attr6Klass1C_Ts5 : $@convention(method) (@guaranteed InternalThing2<Klass1>) -> @owned Klass1 {

    @_specialize(exported: true, target: computedX, where T == Klass1)
    get {
      fatalError("don't call")
    }
  }

  public var specializeComputedY : T {

    // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A14InternalThing2V9computedYxvg", where T == Klass1] [ossa] @$s1A14InternalThing2V15specialize_attrE0C9ComputedYxvg : $@convention(method) <T> (@in_guaranteed InternalThing2<T>) -> @out T {
    // CHECK-OPT-DAG: sil {{.*}}@$s1A14InternalThing2V9computedYxvg15specialize_attr6Klass1C_Ts5 : $@convention(method) (@guaranteed InternalThing2<Klass1>) -> @owned Klass1 {

    @_specialize(exported: true, target: computedY, where T == Klass1)
    get {
      fatalError("don't call")
    }

    // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A14InternalThing2V9computedYxvs", where T == Klass1] [ossa] @$s1A14InternalThing2V15specialize_attrE0C9ComputedYxvs : $@convention(method) <T> (@in T, @inout InternalThing2<T>) -> () {
    // CHECK-OPT-DAG: sil {{.*}}@$s1A14InternalThing2V9computedYxvs15specialize_attr6Klass1C_Ts5 : $@convention(method) (@owned Klass1, @inout InternalThing2<Klass1>) -> () {

    @_specialize(exported: true, target: computedY, where T == Klass1)
    set {}
  }

  public var specializeComputedZ : T {

    // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A14InternalThing2V9computedZxvM", where T == Klass1] [ossa] @$s1A14InternalThing2V15specialize_attrE0C9ComputedZxvM : $@yield_once @convention(method) <T> (@inout InternalThing2<T>) -> @yields @inout T {
    // CHECK-OPT-DAG: sil @$s1A14InternalThing2V9computedZxvM15specialize_attr6Klass1C_Ts5 : $@yield_once @convention(method) (@inout InternalThing2<Klass1>) -> @yields @inout Klass1 {

    @_specialize(exported: true, target: computedZ, where T == Klass1)
    _modify {
      fatalError("don't call")
    }

    // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A14InternalThing2V9computedZxvr", where T == Klass1] [ossa] @$s1A14InternalThing2V15specialize_attrE0C9ComputedZxvr : $@yield_once @convention(method) <T> (@in_guaranteed InternalThing2<T>) -> @yields @in_guaranteed T {
    // CHECK-OPT-DAG: sil @$s1A14InternalThing2V9computedZxvr15specialize_attr6Klass1C_Ts5 : $@yield_once @convention(method) (@guaranteed InternalThing2<Klass1>) -> @yields @in_guaranteed Klass1 {

    @_specialize(exported: true, target: computedZ, where T == Klass1)
    _read {
      fatalError("don't call")
    }
  }

  public subscript(specialized i: Int) -> T {

    // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A14InternalThing2VyxSicig", where T == Klass1] [ossa] @$s1A14InternalThing2V15specialize_attrE11specializedxSi_tcig : $@convention(method) <T> (Int, @in_guaranteed InternalThing2<T>) -> @out T {
    // CHECK-OPT-DAG: sil {{.*}}@$s1A14InternalThing2VyxSicig15specialize_attr6Klass1C_Ts5 : $@convention(method) (Int, @guaranteed InternalThing2<Klass1>) -> @owned Klass1 {

    @_specialize(exported: true, target: subscript(_:), where T == Klass1)
    get {
      fatalError("don't call")
    }

    // CHECK-LABEL: sil [_specialize exported: true, kind: full, target: "$s1A14InternalThing2VyxSicis", where T == Klass1] [ossa] @$s1A14InternalThing2V15specialize_attrE11specializedxSi_tcis : $@convention(method) <T> (@in T, Int, @inout InternalThing2<T>) -> () {
    // CHECK-OPT-DAG: sil {{.*}}@$s1A14InternalThing2VyxSicis15specialize_attr6Klass1C_Ts5 : $@convention(method) (@owned Klass1, Int, @inout InternalThing2<Klass1>) -> () {

    @_specialize(exported: true, target: subscript(_:), where T == Klass1)
    set {
      fatalError("don't call")
    }
  }
}


// Tests for the "public" @specialized attribute.
// CHECK2: @specialized(where T == Int, U == Float)
// CHECK2-NEXT: @specialized(where T == Klass1, U == FakeString)
// CHECK2-NEXT: func publicSpecializeThis<T, U>(_ t: T, u: U)
@specialized(where T == Int, U == Float)
@specialized(where T == Klass1, U == FakeString)
public func publicSpecializeThis<T, U>(_ t: T, u: U) {}
// CHECK2: extension TestPrespecialized {
// CHECK2-NEXT:   @specialized(where T == Int)
// CHECK2-NEXT:   public func testExtension()

extension TestPrespecialized {
    @specialized(where T == Int)
    public func testExtension() {
    }
}

// CHECK2: sil [_specialize exported: false, kind: full, where T == Klass1, U == FakeString] [_specialize exported: false, kind: full, where T == Int, U == Float] [ossa] @$s15specialize_attr20publicSpecializeThis_1uyx_q_tr0_lF
// CHECK2: sil [_specialize exported: false, kind: full, where T == Int] [ossa] @$s15specialize_attr18TestPrespecializedV13testExtensionyyF
