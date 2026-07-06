// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-verbose-sil | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

final class Foo {
  @objc func foo() {}
  // CHECK-LABEL: sil private [thunk] [ossa] @$s10objc_final3FooC3foo{{[_0-9a-zA-Z]*}}FTo

  @objc var prop: Int = 0
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s10objc_final3FooC4propSivgTo
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s10objc_final3FooC4propSivsTo
}

// CHECK-LABEL: sil hidden [ossa] @$s10objc_final7callFooyyAA0D0CF
func callFoo(_ x: Foo) {
  // Calls to the final @objc method statically reference the native entry
  // point.
  // CHECK: function_ref @$s10objc_final3FooC3foo{{[_0-9a-zA-Z]*}}F
  x.foo()

  // Final @objc properties are still accessed directly.
  // CHECK: [[PROP:%.*]] = ref_element_addr {{%.*}} : $Foo, #Foo.prop
  // CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[PROP]] : $*Int
  // CHECK: load [trivial] [[READ]] : $*Int
  let prop = x.prop
  // CHECK: [[PROP:%.*]] = ref_element_addr {{%.*}} : $Foo, #Foo.prop
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[PROP]] : $*Int
  // CHECK: assign {{%.*}} to [[WRITE]] : $*Int
  x.prop = prop
}
