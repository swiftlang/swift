// RUN: %swift -verify -emit-silgen -target x86_64-apple-macosx10.9 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

// Ensure we emit allocating constructor thunks for ObjC initializers that
// were inherited.
// CHECK-LABEL: sil shared @_TFCSo3FooCfMS_FT_S_ : $@thin (@thick Foo.Type) -> @owned Foo {
func foo() {
  var o = Foo()
}

