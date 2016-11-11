// RUN: %target-swift-frontend -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen -emit-verbose-sil | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

final class Foo {
  @objc func foo() {}
  // CHECK-LABEL: sil hidden [thunk] @_TToFC10objc_final3Foo3foo

  @objc var prop: Int = 0
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TToFC10objc_final3Foog4propSi
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TToFC10objc_final3Foos4propSi
}

// CHECK-LABEL: sil hidden @_TF10objc_final7callFooFCS_3FooT_
func callFoo(_ x: Foo) {
  // Calls to the final @objc method statically reference the native entry
  // point.
  // CHECK: function_ref @_TFC10objc_final3Foo3foo
  x.foo()

  // Final @objc properties are still accessed directly.
  // CHECK: [[PROP:%.*]] = ref_element_addr {{%.*}} : $Foo, #Foo.prop
  // CHECK: load [trivial] [[PROP]] : $*Int
  let prop = x.prop
  // CHECK: [[PROP:%.*]] = ref_element_addr {{%.*}} : $Foo, #Foo.prop
  // CHECK: assign {{%.*}} to [[PROP]] : $*Int
  x.prop = prop
}
