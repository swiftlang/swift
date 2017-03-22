// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen | %FileCheck %s

// REQUIRES: objc_interop

import gizmo

// Test mangling of Unicode identifiers.
// These examples are from RFC 3492, which defines the Punycode encoding used
// by name mangling.

// CHECK-LABEL: sil hidden @_T08mangling0022egbpdajGbuEbxfgehfvwxnyyF
func ليهمابتكلموشعربي؟() { }
// CHECK-LABEL: sil hidden @_T08mangling0024ihqwcrbEcvIaIdqgAFGpqjyeyyF
func 他们为什么不说中文() { }
// CHECK-LABEL: sil hidden @_T08mangling0027ihqwctvzcJBfGFJdrssDxIboAybyyF
func 他們爲什麽不說中文() { }
// CHECK-LABEL: sil hidden @_T08mangling0030Proprostnemluvesky_uybCEdmaEBayyF
func Pročprostěnemluvíčesky() { }

// <rdar://problem/13757744> Variadic tuples need a different mangling from
// non-variadic tuples.

// CHECK-LABEL: sil hidden @_T08mangling9r13757744ySaySiG1x_tF
func r13757744(x x: [Int]) {}
// CHECK-LABEL: sil hidden @_T08mangling9r13757744ySaySiG1x_dtF
func r13757744(x x: Int...) {}

// <rdar://problem/13757750> Prefix, postfix, and infix operators need
// distinct manglings.

prefix operator +- {}
postfix operator +- {}
infix operator +- {}

// CHECK-LABEL: sil hidden @_T08mangling2psopyxlF
prefix func +- <T>(a: T) {}
// CHECK-LABEL: sil hidden @_T08mangling2psoPyxlF
postfix func +- <T>(a: T) {}

// CHECK-LABEL: sil hidden @_T08mangling2psoiyx_xtlF
func +- <T>(a: T, b: T) {}

// CHECK-LABEL: sil hidden @_T08mangling2psopyx1a_x1btlF
prefix func +- <T>(_: (a: T, b: T)) {}
// CHECK-LABEL: sil hidden @_T08mangling2psoPyx1a_x1btlF
postfix func +- <T>(_: (a: T, b: T)) {}

infix operator «+» {}

// CHECK-LABEL: sil hidden @_T08mangling007p_qcaDcoiS2i_SitF
func «+»(a: Int, b: Int) -> Int { return a + b }

protocol Foo {}
protocol Bar {}

// Ensure protocol list manglings are '_' terminated regardless of length
// CHECK-LABEL: sil hidden @_T08mangling12any_protocolyypF
func any_protocol(_: Any) {}
// CHECK-LABEL: sil hidden @_T08mangling12one_protocolyAA3Foo_pF
func one_protocol(_: Foo) {}
// CHECK-LABEL: sil hidden @_T08mangling18one_protocol_twiceyAA3Foo_p_AaC_ptF
func one_protocol_twice(_: Foo, _: Foo) {}
// CHECK-LABEL: sil hidden @_T08mangling12two_protocolyAA3Bar_AA3FoopF
func two_protocol(_: Foo & Bar) {}

// Ensure archetype depths are mangled correctly.
class Zim<T> {
  // CHECK-LABEL: sil hidden @_T08mangling3ZimC4zangyx_qd__tlF
  func zang<U>(_: T, _: U) {}
  // CHECK-LABEL: sil hidden @_T08mangling3ZimC4zungyqd___xtlF
  func zung<U>(_: U, _: T) {}
}

// Don't crash mangling single-protocol "composition" types.
// CHECK-LABEL: sil hidden @_T08mangling27single_protocol_compositionyAA3Foo_p1x_tF
func single_protocol_composition(x x: protocol<Foo>) {} // expected-warning {{'protocol<...>' composition syntax is deprecated; join the protocols using '&'}}

// Clang-imported classes and protocols get mangled into a magic 'So' context
// to make collisions into link errors. <rdar://problem/14221244>
// CHECK-LABEL: sil hidden @_T08mangling28uses_objc_class_and_protocolySo8NSObjectC1o_So8NSAnsing_p1ptF
func uses_objc_class_and_protocol(o o: NSObject, p: NSAnsing) {}

// Clang-imported structs get mangled using their Clang module name.
// FIXME: Temporarily mangles everything into the virtual module __C__
// <rdar://problem/14221244>
// CHECK-LABEL: sil hidden @_T08mangling17uses_clang_structySC6NSRectV1r_tF
func uses_clang_struct(r r: NSRect) {}

// CHECK-LABEL: sil hidden @_T08mangling14uses_optionalsScSgSiSg1x_tF
func uses_optionals(x x: Int?) -> UnicodeScalar? { return nil }

enum GenericUnion<T> {
  // CHECK-LABEL: sil shared [transparent] @_T08mangling12GenericUnionO3FooACyxGSicAEmlF
  case Foo(Int)
}

func instantiateGenericUnionConstructor<T>(_ t: T) {
  _ = GenericUnion<T>.Foo
}

struct HasVarInit {
  static var state = true && false
}
// CHECK-LABEL: // function_ref static mangling.HasVarInit.(state : Swift.Bool).(variable initialization expression).(implicit closure #1)
// CHECK-NEXT:  function_ref @_T08mangling10HasVarInitV5stateSbvZfiSbyKXKfu_

// auto_closures should not collide with the equivalent non-auto_closure
// function type.

// CHECK-LABEL: sil hidden @_T08mangling19autoClosureOverloadySiyXK1f_tF : $@convention(thin) (@owned @callee_owned () -> Int) -> () {
func autoClosureOverload(f f: @autoclosure () -> Int) {}
// CHECK-LABEL: sil hidden @_T08mangling19autoClosureOverloadySiyc1f_tF : $@convention(thin) (@owned @callee_owned () -> Int) -> () {
func autoClosureOverload(f f: () -> Int) {}

// CHECK-LABEL: sil hidden @_T08mangling24autoClosureOverloadCallsyyF : $@convention(thin) () -> () {
func autoClosureOverloadCalls() {
  // CHECK: function_ref @_T08mangling19autoClosureOverloadySiyXK1f_tF
  autoClosureOverload(f: 1)
  // CHECK: function_ref @_T08mangling19autoClosureOverloadySiyc1f_tF
  autoClosureOverload {1}
}

// <rdar://problem/16079822> Associated type requirements need to appear in the
// mangling.

protocol AssocReqt {}

protocol HasAssocType {
  associatedtype Assoc
}

// CHECK-LABEL: sil hidden @_T08mangling4fooAyxAA12HasAssocTypeRzlF : $@convention(thin) <T where T : HasAssocType> (@in T) -> ()
func fooA<T: HasAssocType>(_: T) {}
// CHECK-LABEL: sil hidden @_T08mangling4fooByxAA12HasAssocTypeRzAA0D4Reqt0D0RpzlF : $@convention(thin) <T where T : HasAssocType, T.Assoc : AssocReqt> (@in T) -> ()
func fooB<T: HasAssocType where T.Assoc: AssocReqt>(_: T) {}

// CHECK-LABEL: sil hidden @_T08mangling2qqoiySi_SitF
func ??(x: Int, y: Int) {}

struct InstanceAndClassProperty {
  var property: Int {
    // CHECK-LABEL: sil hidden @_T08mangling24InstanceAndClassPropertyV8propertySifg
    get { return 0 }
    // CHECK-LABEL: sil hidden @_T08mangling24InstanceAndClassPropertyV8propertySifs
    set {}
  }
  static var property: Int {
    // CHECK-LABEL: sil hidden @_T08mangling24InstanceAndClassPropertyV8propertySifgZ
    get { return 0 }
    // CHECK-LABEL: sil hidden @_T08mangling24InstanceAndClassPropertyV8propertySifsZ
    set {}
  }
}

// CHECK-LABEL: sil hidden @_T08mangling6curry1yyF : $@convention(thin) () -> ()
func curry1() {

}

// CHECK-LABEL: sil hidden @_T08mangling3barSiyKF : $@convention(thin) () -> (Int, @error Error)
func bar() throws -> Int { return 0 }

// CHECK-LABEL: sil hidden @_T08mangling12curry1ThrowsyyKF : $@convention(thin) () -> @error Error
func curry1Throws() throws {

}

// CHECK-LABEL: sil hidden @_T08mangling12curry2ThrowsyycyKF : $@convention(thin) () -> (@owned @callee_owned () -> (), @error Error)
func curry2Throws() throws -> () -> () {
  return curry1
}

// CHECK-LABEL: sil hidden @_T08mangling6curry3yyKcyF : $@convention(thin) () -> @owned @callee_owned () -> @error Error
func curry3() -> () throws -> () {
  return curry1Throws
}

// CHECK-LABEL: sil hidden @_T08mangling12curry3ThrowsyyKcyKF : $@convention(thin) () -> (@owned @callee_owned () -> @error Error, @error Error)
func curry3Throws() throws -> () throws -> () {
  return curry1Throws
}
