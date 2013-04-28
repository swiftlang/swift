// RUN: %swift -emit-sil %s | FileCheck %s

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

// FIXME: SILGen bug--constructs invalid address-only tuple
//func [prefix] +- <T>(_:(a:T, b:T)) {}
//func [postfix] +- <T>(_:(a:T, b:T)) {}
