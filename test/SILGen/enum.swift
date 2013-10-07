// RUN: %swift -emit-silgen %s | FileCheck %s
protocol P {}

// Regression test for a bug where temporary allocations created as a result of
// tuple implosion were not deallocated in enum constructors.
enum Foo { case A(P, String) }

// CHECK: sil [transparent] @_TO4enum3Foo1AfMS0_FTPS_1P_SS_S0_ : $[thin] ((P, String), Foo.metatype) -> Foo {
// CHECK:   [[ALLOC:%.*]] = alloc_stack $(P, String)
// CHECK:   dealloc_stack [[ALLOC]]#0
// CHECK: }
