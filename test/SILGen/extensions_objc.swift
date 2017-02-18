// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -sdk %S/Inputs %s -I %S/Inputs -enable-source-import -emit-silgen | %FileCheck %s
//
// REQUIRES: objc_interop

import Foundation

class Foo {}

extension Foo {
  dynamic func kay() {}
  dynamic var cox: Int { return 0 }
}

// CHECK-LABEL: sil hidden @_T015extensions_objc19extensionReferencesyAA3FooCF
// CHECK: bb0([[ARG:%.*]] : $Foo):
func extensionReferences(_ x: Foo) {
  // dynamic extension methods are still dynamically dispatched.
  // CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK: class_method [volatile] [[BORROWED_ARG]] : $Foo, #Foo.kay!1.foreign
  x.kay()

  // CHECK: [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
  // CHECK: class_method [volatile] [[BORROWED_ARG]] : $Foo, #Foo.cox!getter.1.foreign
  _ = x.cox

}

func extensionMethodCurrying(_ x: Foo) {
  _ = x.kay
}

// CHECK-LABEL: sil shared [thunk] @_T015extensions_objc3FooC3kayyyFTc
// CHECK:         function_ref @_T015extensions_objc3FooC3kayyyFTD
// CHECK-LABEL: sil shared [transparent] [thunk] @_T015extensions_objc3FooC3kayyyFTD
// CHECK:         bb0([[SELF:%.*]] : $Foo):
// CHECK:           [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:           class_method [volatile] [[SELF_COPY]] : $Foo, #Foo.kay!1.foreign
