// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

func foo(a: Int) {}
func foo(q: String = "", a: Int) {}

// CHECK: function_ref @$s12rdar362268743foo1aySi_tF : $@convention(thin) (Int) -> ()
foo(a: 42)

func bar(a: Int, c: Int) {}
func bar(a: Int, b: Int = 0, c: Int) {}

// CHECK: function_ref @$s12rdar362268743bar1a1cySi_SitF : $@convention(thin) (Int, Int) -> ()
bar(a: 0, c: 42)
