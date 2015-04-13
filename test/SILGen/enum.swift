// RUN: %target-swift-frontend -parse-as-library -parse-stdlib -emit-silgen %s | FileCheck %s

protocol P {}

struct String { var ptr: Builtin.NativeObject }

// Regression test for a bug where temporary allocations created as a result of
// tuple implosion were not deallocated in enum constructors.
enum Foo { case A(P, String) }

// CHECK: sil hidden [transparent] @_TFO4enum3Foo1AfMS0_FTPS_1P_VS_6String_S0_ : $@convention(thin) (@out Foo, @in P, @owned String, @thin Foo.Type) -> () {
// CHECK:   [[ALLOC:%.*]] = alloc_stack $(P, String)
// CHECK:   dealloc_stack [[ALLOC]]#0
// CHECK: }
