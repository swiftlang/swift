// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -emit-silgen -emit-verbose-sil | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

final class Foo {
  @objc func foo() {}
  // CHECK-LABEL: sil hidden [thunk] @_T010objc_final3FooC3foo{{[_0-9a-zA-Z]*}}FTo

  @objc var prop: Int = 0
  // CHECK-LABEL: sil hidden [thunk] @_T010objc_final3FooC4propSifgTo
  // CHECK-LABEL: sil hidden [thunk] @_T010objc_final3FooC4propSifsTo
}

// CHECK-LABEL: sil hidden @_T010objc_final7callFooyAA0D0CF
func callFoo(_ x: Foo) {
  // Calls to the final @objc method statically reference the native entry
  // point.
  // CHECK: function_ref @_T010objc_final3FooC3foo{{[_0-9a-zA-Z]*}}F
  x.foo()

  // Final @objc properties are still accessed directly.
  // CHECK: [[PROP:%.*]] = ref_element_addr {{%.*}} : $Foo, #Foo.prop
  // CHECK: load [trivial] [[PROP]] : $*Int
  let prop = x.prop
  // CHECK: [[PROP:%.*]] = ref_element_addr {{%.*}} : $Foo, #Foo.prop
  // CHECK: assign {{%.*}} to [[PROP]] : $*Int
  x.prop = prop
}
