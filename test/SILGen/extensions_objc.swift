
// RUN: %target-swift-frontend -module-name extensions_objc -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen -enable-sil-ownership | %FileCheck %s
//
// REQUIRES: objc_interop

import Foundation

class Foo {}

extension Foo {
  dynamic func kay() {}
  dynamic var cox: Int { return 0 }
}

// CHECK-LABEL: sil hidden @$S15extensions_objc19extensionReferencesyyAA3FooCF
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

// CHECK-LABEL: sil shared [thunk] @$S15extensions_objc3FooC3kayyyFTc
// CHECK:         function_ref @$S15extensions_objc3FooC3kayyyFTD
// CHECK-LABEL: sil shared [transparent] [serializable] [thunk] @$S15extensions_objc3FooC3kayyyFTD
// CHECK:         bb0([[SELF:%.*]] : @guaranteed $Foo):
// CHECK:           [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:           objc_method [[SELF_COPY]] : $Foo, #Foo.kay!1.foreign
