// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -verify -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

// Ensure we emit allocating constructor thunks for ObjC initializers that
// were inherited.
// CHECK-LABEL: sil shared @_TFCSo3FooCfMS_FT_S_ : $@thin (@thick Foo.Type) -> @owned Foo {
func foo() {
  var o = Foo()
}

