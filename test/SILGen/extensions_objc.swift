
// RUN: %target-swift-emit-silgen -module-name extensions_objc -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -enable-sil-ownership | %FileCheck %s
//
// REQUIRES: objc_interop

import Foundation

class Foo {}

extension Foo {
  @objc dynamic func kay() {}
  @objc dynamic var cox: Int { return 0 }
}

// CHECK-LABEL: sil hidden @$s15extensions_objc19extensionReferencesyyAA3FooCF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Foo):
func extensionReferences(_ x: Foo) {
  // dynamic extension methods are still dynamically dispatched.
  // CHECK: objc_method [[ARG]] : $Foo, #Foo.kay!1.foreign
  x.kay()

  // CHECK: objc_method [[ARG]] : $Foo, #Foo.cox!getter.1.foreign
  _ = x.cox

}

func extensionMethodCurrying(_ x: Foo) {
  _ = x.kay
}

// CHECK-LABEL: sil shared [thunk] @$s15extensions_objc3FooC3kayyyFTc
// CHECK:         function_ref @$s15extensions_objc3FooC3kayyyFTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$s15extensions_objc3FooC3kayyyFTD
// CHECK:         bb0([[SELF:%.*]] : @guaranteed $Foo):
// CHECK:           [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:           objc_method [[SELF_COPY]] : $Foo, #Foo.kay!1.foreign
