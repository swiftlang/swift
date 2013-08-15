// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-silgen | FileCheck %s

import gizmo

// Test mangling of Unicode identifiers.
// These examples are from RFC 3492, which defines the Punycode encoding used
// by name mangling.

// CHECK: sil @_T8manglingX22egbpdajGbuEbxfgehfvwxnFT_T_
func ليهمابتكلموشعربي؟() { }
// CHECK: sil @_T8manglingX24ihqwcrbEcvIaIdqgAFGpqjyeFT_T_
func 他们为什么不说中文() { }
// CHECK: sil @_T8manglingX27ihqwctvzcJBfGFJdrssDxIboAybFT_T_
func 他們爲什麽不說中文() { }
// CHECK: sil @_T8manglingX30Proprostnemluvesky_uybCEdmaEBaFT_T_
func Pročprostěnemluvíčesky() { }

// <rdar://problem/13757744> Variadic tuples need a different mangling from
// non-variadic tuples.

// CHECK: sil @_T8mangling9r13757744FT1xGSaSi__T_
func r13757744(x:Int[]) {}
// CHECK: sil @_T8mangling9r13757744Ft1xGSaSi__T_
func r13757744(x:Int...) {}

// <rdar://problem/13757750> Prefix, postfix, and infix operators need
// distinct manglings.

operator prefix +- {}
operator postfix +- {}
operator infix +- {}

// CHECK: sil @_T8manglingop2psU__FT1aQ__T_
func [prefix] +- <T>(a:T) {}
// CHECK: sil @_T8manglingoP2psU__FT1aQ__T_
func [postfix] +- <T>(a:T) {}

// CHECK: sil @_T8manglingoi2psU__FT1aQ_1bQ__T_
func +- <T>(a:T, b:T) {}

// CHECK: sil @_T8manglingop2psU__FT1aQ_1bQ__T_
func [prefix] +- <T>(_:(a:T, b:T)) {}
// CHECK: sil @_T8manglingoP2psU__FT1aQ_1bQ__T_
func [postfix] +- <T>(_:(a:T, b:T)) {}

operator infix «+» {}

// CHECK: sil @_T8manglingXoi7p_qcaDcFT1aSi1bSi_Si
func «+»(a:Int, b:Int) -> Int { return a + b }

// Curried function entry points mangle in terms of their original types, not
// their uncurried internal SIL types.
// CHECK: sil @_T8mangling7curriedfT1aSi_FT1bSS_T_ : $[thin] ((b : String), (a : Int64)) -> ()
// CHECK: sil @_T8mangling7curriedFT1aSi_FT1bSS_T_ : $[thin] (a : Int64) -> (b : String) -> ()
func curried(a:Int)(b:String) {}

protocol Foo {}
protocol Bar {}

// Ensure protocol list manglings are '_' terminated regardless of length
// CHECK: sil @_T8mangling12any_protocolFP_T_
func any_protocol(_:protocol<>) {}
// CHECK: sil @_T8mangling12one_protocolFPS_3Foo_T_
func one_protocol(_:Foo) {}
// CHECK: sil @_T8mangling18one_protocol_twiceFTPS_3Foo_PS0___T_
func one_protocol_twice(_:Foo, _:Foo) {}
// CHECK: sil @_T8mangling12two_protocolFPS_3BarS_3Foo_T_
func two_protocol(_:protocol<Foo, Bar>) {}

// Ensure archetype depths are mangled correctly.
class Zim<T> {
  // CHECK: sil @_TC8mangling3Zim4zangU__fGS0_Q__U__FTQd__Q__T_
  func zang<U>(_:T, _:U) {}
  // CHECK: sil @_TC8mangling3Zim4zungU__fGS0_Q__U__FTQ_Qd___T_
  func zung<U>(_:U, _:T) {}
}

// Don't crash mangling single-protocol "composition" types.
// CHECK: sil @_T8mangling27single_protocol_compositionFT1xPS_3Foo__T_
func single_protocol_composition(x:protocol<Foo>) {}

// Clang-imported classes and protocols get mangled into a magic 'So' context
// to make collisions into link errors. <rdar://problem/14221244>
// CHECK: sil @_T8mangling28uses_objc_class_and_protocolFT1oCSo8NSObject1pPSo13NSAnsingProto__T_
func uses_objc_class_and_protocol(o:NSObject, p:NSAnsingProto) {}

// Clang-imported structs get mangled using their Clang module name.
// FIXME: Temporarily mangles everything into the virtual module __C__
// <rdar://problem/14221244>
// CHECK: sil @_T8mangling17uses_clang_structFT1rVSC6NSRect_T_
func uses_clang_struct(r:NSRect) {}
