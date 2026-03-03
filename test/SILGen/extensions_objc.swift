
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name extensions_objc -sdk %S/Inputs %s -I %S/Inputs -enable-source-import | %FileCheck %s
//
// REQUIRES: objc_interop

import Foundation

class Foo {}

extension Foo {
  @objc dynamic func kay() {}
  @objc dynamic var cox: Int { return 0 }
}

// CHECK-LABEL: sil hidden [ossa] @$s15extensions_objc19extensionReferencesyyAA3FooCF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Foo):
func extensionReferences(_ x: Foo) {
  // dynamic extension methods are still dynamically dispatched.
  // CHECK: objc_method [[ARG]] : $Foo, #Foo.kay!foreign
  x.kay()

  // CHECK: objc_method [[ARG]] : $Foo, #Foo.cox!getter.foreign
  _ = x.cox

}

func extensionMethodCurrying(_ x: Foo) {
  _ = x.kay
}

// CHECK-LABEL: sil private [ossa] @$s15extensions_objc23extensionMethodCurryingyyAA3FooCFyycADcfu_ : $@convention(thin) (@guaranteed Foo) -> @owned @callee_guaranteed () -> () {
// CHECK: function_ref @$s15extensions_objc23extensionMethodCurryingyyAA3FooCFyycADcfu_yycfu0_ : $@convention(thin) (@guaranteed Foo) -> ()

// CHECK-LABEL: sil private [ossa] @$s15extensions_objc23extensionMethodCurryingyyAA3FooCFyycADcfu_yycfu0_ : $@convention(thin) (@guaranteed Foo) -> () {
// CHECK: objc_method %0 : $Foo, #Foo.kay!foreign : (Foo) -> () -> (), $@convention(objc_method) (Foo) -> ()

